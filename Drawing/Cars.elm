module Drawing.Cars exposing (svgCarBox)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

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
      --, Svg.text_ [x (toString px), y (toString py), color "red"] [Svg.text (debugCarStatus car)]
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
        Left _ -> "yellow"
        Right _ -> "#6060ff"
        Straight -> "white"
