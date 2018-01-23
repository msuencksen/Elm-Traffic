module Logic.Cars exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)

-- check for car movement:
-- Going through lane's car list with foldr,
-- since cars most advanced are at the end of the list.
updateCars: Lane -> Lane
updateCars lane =
  { lane | cars = lane.cars |> List.foldr (checkCarMove lane.direction lane.lights) [] }

-- actually move cars
processCarMove: Lane -> Lane
processCarMove lane =
  { lane | cars = lane.cars |> List.map moveCar |> List.filter (\car -> car.x < (lane.distance+laneWidth)) }

moveCar: Car -> Car
moveCar car =
  case car.canMove > 0 of
    False -> car
    True -> { car |
              x = car.x + car.canMove,
              -- nextCarTurn = Nothing,
              carStatus = Moving
            }


-- Function for use with foldr:
-- current car is checked for
-- a. distance from first car of current cars list
-- b. distance to nearest light
-- c. prepared for lane switch at junctions
--
checkCarMove: Direction -> Lights -> Car -> List Car -> List Car
checkCarMove direction lights car cars =
  let
    carInFront = List.head cars

    abstandCar =
      case carInFront of
        Nothing -> infinity
        Just carInFront ->
          carInFront.x - car.x

    nextTrafficLight =
      lights |> Array.foldl (\light nextLight -> nearestLight car.x light nextLight) Nothing

    carClear1 = (abstandCar > carClearance)

    carLightsDistance =
        case nextTrafficLight of
          Nothing -> infinity
          Just light -> light.p - car.x

    nextLightStraight =
      case nextTrafficLight of
        Nothing -> True
        Just light -> light.straight || carLightsDistance < 0 || carLightsDistance == infinity

    carClearLights1 = carLightsDistance > carClearanceHalf -- still away from lights stop
    carClearLights2 = carLightsDistance < carClearanceHalf -- passed nearest light "yellow"
    carClearLights3 = Maybe.withDefault False (Maybe.map (\light -> not light.on) nextTrafficLight) -- lights are green

    lightsClear = (carClearLights1 || carClearLights2 || carClearLights3)

    junctionJammed =
      case carInFront of
        Nothing -> False
        Just carx -> not carClearLights2 && not carClearLights1 && carClearLights3 && (abstandCar <=  carSpace + 2*laneWidth) -- && carx.carStatus /= Moving

    -- get a turn decision from random number stored with light
    carTurn =
      case car.nextCarTurn of
        Just _ -> if car.nextCarTurn == Just Straight && carLightsDistance < 0 then
                    Nothing -- reset for cars going straight
                  else
                    car.nextCarTurn
        Nothing ->
          if (carLightsDistance > 0 && carLightsDistance < carClearance) then
            case nextTrafficLight of
              Nothing -> Nothing
              Just light ->
                case light.nextCarTurn of
                  Nothing -> Nothing
                  Just carTurn -> Just carTurn
          else
            car.nextCarTurn

    carCanMove = -- bool
      if carClear1 && lightsClear && not junctionJammed then
        if abstandCar > carSpeedClear && car.carStatus == Moving && nextLightStraight && (carLightsDistance > 2*carLength || carClearLights3) then -- && carLightsDistance > carSpeedClear then
          2
        else
          1
      else
        0


    carTurnAngle =
      case car.nextCarTurn of
        Nothing -> 0
        Just Straight -> 0
        Just (Left _) ->
          -- for left turns, move over 1.5 * lane width
          if carLightsDistance < -laneWidth && carLightsDistance >= -( laneWidth + laneHalfWidth) then
            car.turnAngle - carTurnStep
          else
            car.turnAngle
        Just (Right _) ->
          -- for left turns, move over 0.5 * lane width
          if carLightsDistance < 0 && carLightsDistance >= -laneHalfWidth then
            car.turnAngle + carTurnStep
          else
            car.turnAngle

    isTurning = not (carTurn ==  Nothing || carTurn == Just Straight)

    movedCar =
      { car |
         distancePredecessor = abstandCar,
         canMove = carCanMove,
           -- case not isTurning of
           --   True -> carCanMove
           --   False -> 0 ,
         nextCarTurn = carTurn,
         carStatus =
           if not isTurning then
             case (carCanMove > 0, lightsClear) of
               (False, False) -> LightsStop
               (True, False) -> LightsStop
               (False, True) -> JamStop
               (True, True) -> Moving
             else
               if lightsClear then
                 Turning
               else
                 LightsStop
                 ,

         switchNow =
           case carTurn of
              Just (Left _) -> carLightsDistance < -laneHalfWidth
              Just (Right _) -> carLightsDistance < -laneHalfWidth
              _ -> False
         ,
         turnAngle =
          if car.turnAngle > 0 then
            car.turnAngle - carTurnStep
         else
           if car.turnAngle < 0 then
             car.turnAngle + carTurnStep
           else
             car.turnAngle

      }
  in
    movedCar :: cars

-- returns the (absolute) nearest light
nearestLight: Int -> Light -> Maybe Light -> Maybe Light
nearestLight cx light1 light2 =
  case (light1, light2) of
    (l1, Nothing) -> Just l1
    (l1, Just l2) ->
      if abs (l1.p-cx) < abs (l2.p-cx) then
        Just l1
      else
        Just l2
