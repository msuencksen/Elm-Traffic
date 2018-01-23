module Interaction.RandomEffects exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Random exposing (..)
import Initial  exposing (..)

-- generate random numbers for:
-- 1. lane car spawn CarProbability
-- 2. junction turn probability

randomNumbers: Model -> List (Cmd Msg)
randomNumbers model =
  let
    -- probability for new car in lane
    laneRandomCar = Random.generate CarProbability (Random.list (Array.length model.lanes) (Random.float 0 1))

    -- probability for turns at junction lights
    laneRandomTurns = Array.toList (Array.indexedMap (\laneId lane -> (Random.generate (TurnProbability laneId) (Random.list (Array.length lane.lights) (Random.float 0 1)) ) ) model.lanes)

  in
    laneRandomCar :: laneRandomTurns


updateTurns: Int -> Array Lane -> Array Float -> Array Lane
updateTurns laneId lanes randomFloats =
  let
    getLane = Array.get laneId lanes
  in
    case getLane of
      Nothing -> lanes
      Just lane ->
        let
          lightsZippedWithRandom = Array.Extra.map2 (,) lane.lights randomFloats
        in
          lanes |> Array.set laneId { lane | lights = Array.Extra.map2 probeNextTurn lane.lights randomFloats}

probeNextTurn: Light -> Float -> Light
probeNextTurn light randomFloat =
  { light | nextCarTurn =
    case (light.left, light.right, light.straight) of
      (Nothing, Nothing, False) -> Nothing
      (Nothing, Nothing, True) -> Just Straight
      (Just l, Nothing, False) -> Just (Left l)
      (Nothing, Just r, False) -> Just (Right r)
      (Just l, Nothing, True) -> if (randomFloat < 0.33) then Just (Left l) else Just Straight -- left or straight
      (Nothing, Just r, True) -> if (randomFloat < 0.33) then Just (Right r) else Just Straight -- right or straight
      (Just l, Just r, False) -> if (randomFloat < 0.5) then Just (Left l) else Just (Right r) -- left or right
      (Just l, Just r, True) -> if (randomFloat < 0.33) then Just (Left l) else if (randomFloat > 0.66) then Just (Right r) else Just Straight -- left/right/straight
  }


-- add possible new car, update backlog
addNewCar: (Lane,Float) -> Lane
addNewCar (lane,probability) =
  let
    distance =
      case (List.head lane.cars) of
        Nothing -> infinity
        Just car -> car.x
    carToAdd =
      if lane.spawn && probability > 0.991 then
        1
      else
        0
  in
    if (carToAdd > 0 || lane.carBacklog > 0) then
      if distance < carHalfLength + carSpace then
         { lane | carBacklog = lane.carBacklog + carToAdd } -- no free space for car
      else
        { lane | cars = initialCar :: lane.cars,
                 carBacklog = lane.carBacklog + carToAdd - 1} -- get from backlog
    else
      lane -- nothing to do
