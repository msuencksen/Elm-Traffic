module TrafficDrawing exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
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
      if lane.direction.dx /= 0 then
        svgLane lane.startCoord.x (lane.startCoord.y-laneHalfWidth) (lane.endCoord.x - lane.startCoord.x) laneWidth
      else
        svgLane (lane.startCoord.x-laneHalfWidth) (lane.startCoord.y) laneWidth (lane.endCoord.y - lane.startCoord.y)

  in
    [laneConcrete]

-- svgLane
svgLane: Int -> Int -> Int -> Int -> Svg Msg
svgLane px py w h =
  rect [ x (toString px), y (toString py), Svg.Attributes.width (toString w), Svg.Attributes.height (toString h), fill "#505050" ] []

svgCarBox : Lane -> Car -> List (Svg Msg)
svgCarBox lane car =
  let
    px =
      case lane.direction.dx of
        (1) -> car.x - carHalfLength -- car position on lane going east
        (-1) -> lane.endCoord.x - car.x - carHalfLength -- car position on lane going west
        _ -> lane.startCoord.x - carHalfWidth + (turnDeltaCar car lane.direction.dy) -- fixed horizontal position for lane going south or north

    py =
      case lane.direction.dy of
        (1) -> car.x - carHalfLength -- position on lane going south
        (-1) -> lane.endCoord.y - car.x - carHalfLength -- position on lane going north
        _ -> lane.startCoord.y - carHalfWidth + (turnDeltaCar car lane.direction.dx) -- fixed vertical position for lane going south or north

    boxWidth =
      if lane.direction.dx /= 0 then
        carLength
      else
        carWidth

    boxHeight =
      if lane.direction.dx /= 0 then
        carWidth
      else
        carLength
  in
    [ svgCar1 px py boxWidth boxHeight car.turnAngle (svgCarColor car.nextCarTurn)
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


turnDeltaCar: Car -> Int -> Int
turnDeltaCar car direction =
  if car.turnAngle < 0 then
    (car.turnAngle // carTurnStep) * 1 -- direction
  else
    (car.turnAngle // carTurnStep) * 1 -- direction

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