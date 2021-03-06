module Logic.LaneSwitch exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)


-- switch turning cars (carStatus == Turning) to their next lane
processCarLaneSwitch: Array Lane -> Array Lane
processCarLaneSwitch lanes =
  let
    switchFromLaneFuncs = Array.indexedMap (\laneId lane -> switchCarsFromTo laneId lane) lanes -- create array of functions
  in
    switchFromLaneFuncs |> Array.foldr (\switchFunc allLanes-> switchFunc allLanes) lanes

-- processCarLaneSwitch: Array Lane -> Array Lane
-- processCarLaneSwitch lanes =
--   let
--     lane0 = Array.get 0 lanes
--     lane1 = Array.get 1 lanes
--     lane2 = Array.get 2 lanes
--     lane3 = Array.get 3 lanes
--     lane4 = Array.get 4 lanes
--   in
--     case (lane0,lane1,lane2,lane3,lane4) of
--       (Just l0, Just l1, Just l2, Just l3, Just l4) ->
--         lanes |> switchCarsFromTo 0 l0 |> switchCarsFromTo 1 l1 |>switchCarsFromTo 2 l2 |> switchCarsFromTo 3 l3 |> switchCarsFromTo 4 l4
--       _ -> lanes

-- try to switch turning cars
switchCarsFromTo: Int -> Lane -> Array Lane -> Array Lane
switchCarsFromTo fromLaneId fromLane allLanes =
  let
    --switchCar = fromLane.cars |> List.filter (\car -> car.switchNow ) |> List.head
    switchCar = fromLane.cars |> List.filter (\car -> (car.carStatus == Turning || car.carStatus == WaitLeftTurn || car.carStatus == WaitRightTurn) && car.nextCarTurn /= Nothing && car.nextCarTurn /= Just Straight ) |> List.head
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

          turnTo =
            case car.nextCarTurn of
              Just (Left l) -> TurnToLeft
              _ -> TurnToRight

          possibleToLane = -- ! might fail from config error !
            Array.get toLaneId allLanes
        in
          case possibleToLane of
            Nothing -> allLanes
            Just toLane ->
              let
                entryPoint = entryAtJunction fromLane toLane

                free = entryAtJunctionFree toLane entryPoint

                incomingLane = -- contraflow
                  case car.nextCarTurn of
                    Just (Left l) -> Array.get fromLane.oppositeLane allLanes
                    _ -> Nothing

                waitForIncoming =
                  case incomingLane of
                    Nothing -> False
                    Just oppositeLane -> not (entryAtLeftTurnFree oppositeLane (oppositeLane.distance - car.x - 2*laneWidth)) -- entryPoint d-car.p

              in
                if (car.switchNow && free && not waitForIncoming) then
                  allLanes |> Array.set toLaneId (addCarToLane toLane car entryPoint turnTo)
                           |> Array.set fromLaneId (removeCarFromLane fromLane car.x)
                else
                  allLanes |> Array.set fromLaneId (updateWaitingCar fromLane car free waitForIncoming)


entryAtJunction: Lane -> Lane -> Int
entryAtJunction fromLane toLane =
  case toLane.direction of
    East -> fromLane.startCoord.x + laneHalfWidth -- to east->west lane
    West -> toLane.endCoord.x - fromLane.startCoord.x + laneHalfWidth-- to e<-w lane
    South -> fromLane.startCoord.y + laneHalfWidth  -- to n->s lane
    North -> toLane.endCoord.y - fromLane.startCoord.y + laneHalfWidth -- to s->n lane

-- return true if no car.x is in carClearance inverval near p
entryAtJunctionFree: Lane -> Int -> Bool
entryAtJunctionFree lane p =
  let
    gapBegin = p - carClearance
    gapEnd = p + carClearance
    carsNear = lane.cars |> List.filter (\car -> car.x > gapBegin && car.x < gapEnd )
  in
    (List.length carsNear)  == 0

entryAtLeftTurnFree: Lane -> Int -> Bool
entryAtLeftTurnFree oppositeLane p =
  let
    gapBegin = p
    gapEnd = p + 2 * laneWidth
    carsNear = oppositeLane.cars |> List.filter (\car -> car.carStatus /= WaitLeftTurn && car.x > gapBegin && car.x < gapEnd )
  in
    (List.length carsNear)  == 0

updateWaitingCar: Lane -> Car -> Bool -> Bool -> Lane
updateWaitingCar lane waitingCar free waitForIncoming =
  let
    updatedCar =
      { waitingCar | canMove =
                       if waitingCar.switchNow || waitingCar.carStatus /= Turning then
                         0
                       else
                         1
                     ,carStatus =
                       case (free, waitForIncoming) of
                         (False, False) -> WaitRightTurn
                         (True, False) -> JamStop -- never reached currently
                         (False, True) -> WaitLeftTurn
                         (True, True) -> WaitLeftTurn
      }
  in
    { lane
      | cars = (List.filter (\car -> car.x < waitingCar.x) lane.cars) ++ [updatedCar] ++ (List.filter (\car -> car.x > waitingCar.x) lane.cars)
    }

addCarToLane: Lane -> Car -> Int -> TurnTo -> Lane
addCarToLane lane car p turnTo =
  let
    switchedCar =
      { car |
         nextCarTurn = Nothing,
         x = p,
         turnAngle =
           case turnTo of
             TurnToLeft -> 90
             TurnToRight -> -90,
         carStatus = TurningIn}
  in
    { lane
      | cars = (List.filter (\car -> car.x < p) lane.cars) ++ [switchedCar] ++ (List.filter (\car -> car.x > p) lane.cars)
    }

removeCarFromLane: Lane -> Int -> Lane
removeCarFromLane lane p =
  { lane
    | cars = lane.cars |> List.filter (\car -> car.x /= p)
  }
