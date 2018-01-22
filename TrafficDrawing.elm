module TrafficDrawing exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Random exposing (..)
import Time exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)
import LaneSwitch exposing (..)
import Types exposing (..)
import Constants exposing (..)


drawStreet: Street -> List (Svg Msg)
drawStreet street =
  let
    concrete =
      case street.streetDirection of
        EastWest -> svgLane street.startCoord.x (street.startCoord.y-laneWidth) street.distance (2*laneWidth)
        NorthSouth -> svgLane (street.startCoord.x-laneWidth) street.startCoord.y (2*laneWidth) street.distance
   in
     [concrete]

drawLaneElements: Lane -> List (Svg Msg)
drawLaneElements lane =
  let
    laneConcrete =
      if lane.direction == East || lane.direction == West then
        svgLane lane.startCoord.x (lane.startCoord.y-laneHalfWidth) (lane.endCoord.x - lane.startCoord.x) laneWidth
      else
        svgLane (lane.startCoord.x-laneHalfWidth) (lane.startCoord.y) laneWidth (lane.endCoord.y - lane.startCoord.y)
  in
    [laneConcrete]

svgLane: Int -> Int -> Int -> Int -> Svg Msg
svgLane px py w h =
  rect [ x (toString px), y (toString py), Svg.Attributes.width (toString w), Svg.Attributes.height (toString h), fill "#505050" ] []


drawLaneBacklog: Lane -> List (Svg Msg)
drawLaneBacklog lane =
  let
    fontSize =
      if lane.carBacklog < 5 then
        "12px"
      else
        if lane.carBacklog > 15 then
          "48px"
        else
          "24px"

    textPoint =
      case lane.direction of
        East -> (lane.startCoord.x+15,lane.startCoord.y + laneWidth +15)
        West -> (lane.startCoord.x+lane.distance-25,lane.startCoord.y - laneWidth)
        South ->  (lane.startCoord.x-laneWidth - 25,lane.startCoord.y + 25)
        North -> (lane.startCoord.x+laneWidth,lane.startCoord.y+lane.distance - 15)
  in
    [Svg.text_ [x (toString (Tuple.first textPoint)), y (toString (Tuple.second textPoint )),
               fill "red", Svg.Attributes.fontSize fontSize]
              [Svg.text (toString lane.carBacklog)]
    ]

-- called from html view
drawLightElements: Int -> Lane -> List (Svg Msg)
drawLightElements laneId lane =
  lane.lights |> Array.indexedMap (\lightNo light -> svgLight lightNo light laneId lane.startCoord lane.direction lane.distance ) |> Array.toList |> List.foldr (++) []

svgLight: Int-> Light -> Int -> Point -> Direction -> Int -> List (Svg Msg)
svgLight lightId light laneId laneStart dir distance =
  let
    lightPos =
      case dir of
        East -> { x=laneStart.x + light.p ,
                   y=laneStart.y + laneWidth }
        West -> { x=laneStart.x + distance - light.p ,
                    y=laneStart.y - laneWidth }
        South -> { x=laneStart.x - laneWidth ,
                   y=laneStart.y + light.p - lightStreetSpacing}
        North -> { x=laneStart.x + laneWidth,
                    y=laneStart.y + distance - light.p }-- + 2* lightStreetSpacing}


    lightRotation =
      case dir of
        East -> 90
        West -> -90
        South -> 180
        North -> 0

    rotationTransform = "rotate("++ (toString lightRotation) ++"," ++ (toString (lightPos.x)) ++ "," ++ (toString (lightPos.y)) ++")"

    lightSvgBox = rect [ x (toString lightPos.x),
              y (toString lightPos.y),
              Svg.Attributes.width (toString lightWidth),
              Svg.Attributes.height (toString lightHeight),
              transform (rotationTransform),
              onClick (SwitchLight laneId lightId),
              fill "#303030" ] []

    lightRedSvg = circle [ cx (toString (lightPos.x+lightFireCenterBothX)),
                           cy (toString (lightPos.y+lightFireCenterBothX)),
                           r (toString lightFireRadius),
                           onClick (SwitchLight laneId lightId),
                           transform (rotationTransform),
                           fill (lightFireFill (Red light.on))  ] []
    lightGreenSvg = circle [ cx (toString (lightPos.x+lightFireCenterBothX)),
                             cy (toString (lightPos.y+lightFireCenterGreenY)),
                             r (toString lightFireRadius),
                             onClick (SwitchLight laneId lightId),
                             transform (rotationTransform),
                             fill (lightFireFill (Green (not light.on))) ] []
  in
    [lightSvgBox, lightRedSvg, lightGreenSvg]


lightFireFill: LightFire -> String
lightFireFill lightfire =
  case lightfire of
    Red False -> "#770000"
    Red True -> "#ff0000"
    Green False -> "#007700"
    Green True -> "#00ff00"

-- svgLane
svgCarBox : Lane -> Car -> List (Svg Msg)
svgCarBox lane car =
  let
    px =
      case lane.direction of
        East -> car.x - carHalfLength -- car position on lane going east
        West -> lane.endCoord.x - car.x - carHalfLength -- car position on lane going west
        _ -> 2+ lane.startCoord.x - laneHalfWidth  -- fixed horizontal position for lane going south or north

    py =
      case lane.direction of
        South -> car.x - laneHalfWidth -- position on lane going south
        North -> lane.endCoord.y - car.x - laneHalfWidth -- position on lane going north
        _ -> lane.startCoord.y - carHalfWidth  -- fixed vertical position for lane going south or north

    boxWidth = carLength
    boxHeight = carWidth

    boxRotationAngle =
      let
        laneRotationAngle =
          case lane.direction of
            East -> 0
            West -> 180
            North -> 90
            South -> -90
      in
        laneRotationAngle + car.turnAngle

  in
    [ svgCar1 px py boxWidth boxHeight boxRotationAngle (svgCarColor car.nextCarTurn)
    , Svg.text_ [x (toString px), y (toString py), color "red"] [] -- [Svg.text (debugCarStatus car)]
    ]

debugCarStatus: Car -> String
debugCarStatus car =
  case car.carStatus of
    Moving -> "~"
    Turning -> "+"
    JamStop -> "j"
    LightsStop -> "|"
    WaitLeftTurn -> "wl"
    TurningIn -> "ti"

debugCarDistance: Car -> String
debugCarDistance car =
  if car.distancePredecessor == infinity then
    ""
  else
    toString car.distancePredecessor

debugCarCanMove: Car -> String
debugCarCanMove car =
  if car.canMove > 0 then
    ">"
  else
    "|"



-- Svg Car
svgCar1 : Int -> Int -> Int -> Int -> Int -> String -> Svg Msg
svgCar1 px py w h rotationAngle carColorStr =
    let
      transformAttribute =
        if rotationAngle /= 0 then
          [ transform ("rotate("++ (toString rotationAngle) ++"," ++ (toString (px + w//2)) ++ "," ++ (toString (py+h//2)) ++")" ) ]
        else
          []
    in
      rect ([ x (toString px),
             y (toString py),
             Svg.Attributes.width (toString w),
             Svg.Attributes.height (toString h),
             --transform ("rotate("++ (toString rotationAngle) ++"," ++ (toString (px+ w//2)) ++ "," ++ (toString (py+h//2)) ++")" )
             fill carColorStr ] ++ transformAttribute) []


coord: Int -> Int -> String
coord x y =
  (toString x)++","++(toString y)++" "

svgCarColor: Maybe CarTurn -> String
svgCarColor maybeCarTurn =
  case maybeCarTurn of
    Nothing -> "grey"
    Just carTurn ->
      case carTurn of
        Left _ -> "green"
        Right _ -> "red"
        Straight -> "white"
