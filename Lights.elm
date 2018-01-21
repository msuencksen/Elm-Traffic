module Lights exposing (findSwitchClusters)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)


findSwitchClusters: Array Lane -> Array Lane
findSwitchClusters allLanes =
  allLanes |> Array.indexedMap (updateLaneLights allLanes)

updateLaneLights: Array Lane -> Int -> Lane -> Lane
updateLaneLights allLanes laneId lane =
  { lane |
     lights = lane.lights
       |> Array.map (\light ->
          { light |
              leftLightIndex = (findLeftLight allLanes lane laneId light),
              rightLightIndex = (findRightLight allLanes lane laneId light),
              oppositeLightIndex = (findOppositeLight allLanes lane laneId light)
          } )
  }

findOppositeLight: Array Lane -> Lane -> Int -> Light -> Maybe LightIndex
findOppositeLight allLanes lane laneId thisLight =
  let
    maybeOppositeLane = Array.get lane.oppositeLane allLanes
  in
    case maybeOppositeLane of
      Nothing -> Nothing
      Just oppositeLane ->
        Array.toIndexedList oppositeLane.lights
        |> List.filter (isOppositeLight thisLight)
        |> List.map (\(lightIndex, _) -> { laneId = lane.oppositeLane, lightId=lightIndex })
        |> List.head

-- opposite check for being the same junction
isOppositeLight: Light -> (Int, Light) -> Bool
isOppositeLight l1 (_, l2) =
   (l2.left == l1.right && l1.right /= Nothing)
   || (l2.right == l1.left && l1.left /= Nothing)


-- get our left-turn lane and filter its lights array for a left-turn to our contraflow lane or a right turn to us
findRightLight: Array Lane -> Lane -> Int -> Light -> Maybe LightIndex
findRightLight allLanes lane laneId thisLight =
  let
    rightLaneId =
      case thisLight.right of
        Nothing -> -1
        Just rightLaneIndex -> rightLaneIndex

    leftLaneId = -- we need the opposite of the right lane b/c the light is there. But we could not use light.left directly b/c the junction might be a T junction with no left turn.
       case Array.get rightLaneId allLanes of
         Nothing -> -1
         Just rlane -> rlane.oppositeLane

    maybeLeftLane =
      Array.get leftLaneId allLanes
  in
    case maybeLeftLane of
      Nothing -> Nothing
      Just leftLane ->
        Array.toIndexedList leftLane.lights
        |> List.filter (isRightLight laneId lane.oppositeLane)
        |> List.map (\(lightIndex, _) -> { laneId = leftLaneId, lightId=lightIndex })
        |> List.head

-- left side check for begin the same junction
isRightLight: Int -> Int -> (Int, Light) -> Bool
isRightLight thisLaneId oppositeLaneId (l2Id, l2) =
  l2.right == Just thisLaneId
   || l2.left == Just oppositeLaneId

-- get our right-turn lane and filter its lights array for a right-turn to our contraflow or a left turn to us
findLeftLight: Array Lane -> Lane -> Int -> Light -> Maybe LightIndex
findLeftLight allLanes lane laneId thisLight =
  let
    leftLaneId =
      case thisLight.left of
        Nothing -> -1
        Just leftLaneIndex -> leftLaneIndex

    rightLaneId = -- we need the opposite of the left lane b/c the light is there. But we could not use light.right directly b/c the junction might be a T junction with no left turn.
       case Array.get leftLaneId allLanes of
         Nothing -> -1
         Just rlane -> rlane.oppositeLane

    maybeRightLane =
       Array.get rightLaneId allLanes
  in
    case maybeRightLane of
      Nothing -> Nothing
      Just rightLane ->
        Array.toIndexedList rightLane.lights
        |> List.filter (isLeftLight laneId lane.oppositeLane)
        |> List.map (\(lightIndex, _)-> { laneId = rightLaneId, lightId=lightIndex })
        |> List.head


-- left side check for begin the same junction
isLeftLight: Int -> Int -> (Int, Light) -> Bool
isLeftLight thisLaneId oppositeLaneId (l2id, l2) =
  l2.left == Just thisLaneId
   || l2.right == Just oppositeLaneId
