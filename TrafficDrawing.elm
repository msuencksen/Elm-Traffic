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

drawLaneBacklog: Lane -> List (Svg Msg)
drawLaneBacklog lane =
  let
    textPoint =
      case lane.direction of
        East -> (lane.startCoord.x+15,lane.startCoord.y + laneWidth +15)
        West -> (lane.startCoord.x+lane.distance-15,lane.startCoord.y - laneWidth)
        South ->  (lane.startCoord.x-laneWidth - 15,lane.startCoord.y + 15)
        North -> (lane.startCoord.x+laneWidth,lane.startCoord.y+lane.distance - 15)
  in
    [Svg.text_ [x (toString (Tuple.first textPoint)), y (toString (Tuple.second textPoint )),
               fill "red"]
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
        East -> { x=laneStart.x + light.p - lightStreetSpacing,
                   y=laneStart.y + laneWidth }
        West -> { x=laneStart.x + distance - light.p + lightStreetSpacing,
                    y=laneStart.y - laneWidth }
        South -> { x=laneStart.x - 2* laneWidth - lightStreetSpacing ,
                   y=laneStart.y + light.p - lightHeight - lightStreetSpacing}
        North -> { x=laneStart.x + laneWidth,
                    y=laneStart.y + distance - light.p + 2* lightStreetSpacing}


    lightRotation =
      case dir of
        East -> 90
        West -> -90
        _ -> 0

    rotationTransform = "rotate("++ (toString lightRotation) ++"," ++ (toString (lightPos.x)) ++ "," ++ (toString (lightPos.y)) ++")"

    lightSvgBox = rect [ x (toString lightPos.x),
              y (toString lightPos.y),
              Svg.Attributes.width (toString lightWidth),
              Svg.Attributes.height (toString lightHeight),
              transform (rotationTransform),
              onClick (SwitchLight laneId lightId),
              fill "blue" ] []

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
svgLane: Int -> Int -> Int -> Int -> Svg Msg
svgLane px py w h =
  rect [ x (toString px), y (toString py), Svg.Attributes.width (toString w), Svg.Attributes.height (toString h), fill "#505050" ] []

svgCarBox : Lane -> Car -> List (Svg Msg)
svgCarBox lane car =
  let
    px =
      case lane.direction of
        East -> car.x - carHalfLength -- car position on lane going east
        West -> lane.endCoord.x - car.x - carHalfLength -- car position on lane going west
        _ -> lane.startCoord.x - carHalfWidth  -- fixed horizontal position for lane going south or north

    py =
      case lane.direction of
        South -> car.x - carHalfLength -- position on lane going south
        North -> lane.endCoord.y - car.x - carHalfLength -- position on lane going north
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
    , Svg.text_ [x (toString px), y (toString py), color "red"] [Svg.text (debugCarStatus car)]
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
  if car.canMove then
    ">"
  else
    "|"



-- Svg Car
svgCar1 : Int -> Int -> Int -> Int -> Int -> String -> Svg Msg
svgCar1 px py w h rotationAngle carColorStr =
    rect [ x (toString px),
           y (toString py),
           Svg.Attributes.width (toString w),
           Svg.Attributes.height (toString h),
           transform ("rotate("++ (toString rotationAngle) ++"," ++ (toString (px+ w//2)) ++ "," ++ (toString (py+h//2)) ++")" )
           ,fill carColorStr ] []

svgCar2 : Int -> Int -> Int -> Int -> Int -> String -> Svg Msg
svgCar2 px py w h rotationAngle carColorStr =
   polygon [ Svg.Attributes.points ((coord px py) ++ (coord (px+w) py) ++ (coord (px+w) (py+h)) ++ (coord px (py+h)) ),
          transform ("rotate("++ (toString rotationAngle) ++"," ++ (toString (px+ carHalfLength)) ++ "," ++ (toString (py+carHalfWidth)) ++")" )
           ,fill carColorStr] [
                               rect [ x "0",
                                      y "0",
                                      Svg.Attributes.width ("2"),
                                      Svg.Attributes.height ("22"),
                                      fill "red" ] []
                              ]

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
