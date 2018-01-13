module LaneSwitch exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)

--
-- processCarLaneSwitch: Array Lane -> Array Lane
-- processCarLaneSwitch lanes =
--   let
--     switchFromLaneFuncs = Array.indexedMap (\laneId lane -> switchCarsFromTo laneId lane) lanes -- array of function
--   in
--     Array.foldr (\switchFunc allLanes-> switchFunc allLanes) lanes switchFromLaneFuncs

processCarLaneSwitch: Array Lane -> Array Lane
processCarLaneSwitch lanes =
  let
    lane0 = Array.get 0 lanes
    lane1 = Array.get 1 lanes
    lane2 = Array.get 2 lanes
    lane3 = Array.get 3 lanes
  in
    case (lane0,lane1,lane2,lane3) of
      (Just l0, Just l1, Just l2, Just l3) ->
        lanes |> switchCarsFromTo 0 l0 |> switchCarsFromTo 1 l1 |>switchCarsFromTo 2 l2 |> switchCarsFromTo 3 l3
      _ -> lanes

entryAtJunction: Lane -> Lane -> Int
entryAtJunction fromLane toLane =
  case (toLane.direction.dx, toLane.direction.dy) of
    (1,0) -> fromLane.startCoord.x + laneHalfWidth -- to east->west lane
    (-1,0) -> toLane.endCoord.x - fromLane.startCoord.x - laneHalfWidth-- to e<-w lane
    (0,1) -> fromLane.startCoord.y + laneHalfWidth  -- to n->s lane
    (0,-1) -> toLane.endCoord.y - fromLane.startCoord.y - laneHalfWidth - laneWidth -- to s->n lane
    _ -> 0

-- return true if no car.x is in carClearance inverval near p
entryAtJunctionFree: Lane -> Int -> Bool
entryAtJunctionFree lane p =
  let
    carsNear = lane.cars |> List.filter (\car -> car.x > (p - carClearanceHalf) && car.x < (p + carClearanceHalf) )
  in
    (List.length carsNear)  == 0

switchCarsFromTo: Int -> Lane -> Array Lane -> Array Lane
switchCarsFromTo fromLaneId fromLane allLanes =
  let
    switchCar = fromLane.cars |> List.filter (\car -> car.canMove && car.nextCarTurn /= Nothing && car.nextCarTurn /= Just Straight ) |> List.head
  in
    case switchCar of
      Nothing -> allLanes
      Just car ->
        let
          toLaneId =
            case car.nextCarTurn of
              Just (Left l) -> l
              Just (Right r) -> r
              _ -> -1
          possibleToLane = -- ! might fail from config error !
            Array.get toLaneId allLanes
        in
          case possibleToLane of
            Nothing -> allLanes
            Just toLane ->
              let
                entryPoint = entryAtJunction fromLane toLane
                free = entryAtJunctionFree toLane entryPoint
                switchedCar = { car | x= entryPoint, nextCarTurn = Nothing }
              in
                if free then
                  allLanes |> Array.set toLaneId (addCarToLane toLane car entryPoint)
                           |> Array.set fromLaneId (removeCarFromLane fromLane car.x)
                else
                  allLanes


addCarToLane: Lane -> Car -> Int -> Lane
addCarToLane lane car p =
  let
    switchedCar =
      { car | nextCarTurn = Nothing, x = p}
  in
    { lane
      | cars = (List.filter (\car -> car.x < p) lane.cars) ++ [switchedCar] ++ (List.filter (\car -> car.x > p) lane.cars)
    }

removeCarFromLane: Lane -> Int -> Lane
removeCarFromLane lane p =
  { lane
    | cars = lane.cars |> List.filter (\car -> car.x /= p)
  }
