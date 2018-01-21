module Intersections exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

-- Compute intersections of all laneStart

-- Note: depends for T junctions on laneWidth constants

createLights: Array Lane -> Array Lane
createLights allLanes =
    allLanes |> Array.indexedMap (matchLaneAgainstAll allLanes)

-- match one lane against all lanes
matchLaneAgainstAll: Array Lane -> Int -> Lane -> Lane
matchLaneAgainstAll allLanes laneId lane =
  let
    trafficLights = allLanes |> Array.indexedMap (laneIntersect laneId lane) |> Array.toList |> List.filterMap identity |> Array.fromList
  in
    {lane | lights = trafficLights}

-- match lane1 against lane1, maybe generating a light for lane1
laneIntersect: Int -> Lane -> Int -> Lane -> Maybe Light
laneIntersect l1id l1 l2id l2 =
  let
   defaultLight = {on=True, p=0, left=Nothing, right=Nothing, straight=False, nextCarTurn=Nothing, leftLightIndex=Nothing, rightLightIndex=Nothing, oppositeLightIndex=Nothing }
   margin = laneHalfWidth
  in
    case (l1.direction, l2.direction) of
      (East,South) ->   -- light for E in E+S
        let
          intersect = l2.startCoord.x - l1.startCoord.x
          goesStraight = not (intersect == l1.distance) -- offset ?
          goesLeft = l1.startCoord.y <= l2.endCoord.y + laneWidth && l1.startCoord.y > l2.endCoord.y - l2.distance
          goesRight = (l2.startCoord.y + l2.distance) > l1.startCoord.y
          leftTurn =
            case goesLeft of
              True -> Just l2.oppositeLane
              False -> Nothing
          rightTurn =
            case goesRight of
              True -> Just l2id
              False -> Nothing
        in
          if intersect > 0 && intersect <= l2.distance then
            Just { defaultLight | straight=goesStraight, p = intersect - margin, left=leftTurn, right=rightTurn, straight = goesStraight}
          else
           Nothing

      (West,North) -> -- light for W in W+N
        let
          intersect = l1.endCoord.x - l2.startCoord.x
          goesStraight = not (intersect == l2.startCoord.x)
          goesLeft = (l2.startCoord.y + l2.distance) > l1.startCoord.y
          goesRight = l1.startCoord.y <= l2.endCoord.y + laneWidth && l1.startCoord.y > l2.endCoord.y - l2.distance
          leftTurn =
            case goesLeft of
              True -> Just l2.oppositeLane
              False -> Nothing
          rightTurn =
            case goesRight of
              True -> Just l2id
              False -> Nothing
        in
          if intersect > 0 && intersect <= l2.distance then
            Just { defaultLight | straight=goesStraight, p = intersect - margin, left=leftTurn, right=rightTurn, straight = goesStraight}
          else
           Nothing

      (South,West) -> -- light for S in SxW
        let
          intersectX = l1.startCoord.x - l2.startCoord.x
          intersectY = l2.startCoord.y - l1.startCoord.y
          intersecting = (intersectX >= 0 && intersectX <= l2.distance + laneWidth) && (intersectY > 0 && intersectY <= l1.distance)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight = not (intersectY == l2.startCoord.y)
                goesLeft = (l2.startCoord.x + l2.distance) > l1.startCoord.x
                goesRight = l1.startCoord.x <= l2.endCoord.x + laneWidth && l1.startCoord.x > l2.endCoord.x - l2.distance
                leftTurn =
                  case goesLeft of
                    True -> Just l2.oppositeLane
                    False -> Nothing
                rightTurn =
                  case goesRight of
                    True -> Just l2id
                    False -> Nothing
              in
                Just { defaultLight | straight=goesStraight, p = intersectY - margin, left=leftTurn, right=rightTurn, straight = goesStraight}


      (North,East) -> -- light for N in NxE
        let
          intersectX = l1.startCoord.x - l2.startCoord.x
          intersectY = l2.startCoord.y - l1.startCoord.y
          intersecting = (intersectX >= 0 && intersectX <= l2.distance + laneWidth) && (intersectY > 0 && intersectY <= l1.distance)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight = not (intersectY == l2.startCoord.y)
                goesLeft = l1.startCoord.x <= l2.endCoord.x + laneWidth && l1.startCoord.x > l2.startCoord.x
                goesRight = (l2.startCoord.x + l2.distance) > l1.startCoord.x
                leftTurn =
                  case goesLeft of
                    True -> Just l2.oppositeLane
                    False -> Nothing
                rightTurn =
                  case goesRight of
                    True -> Just l2id
                    False -> Nothing
              in
                Just { defaultLight | straight=goesStraight, p = l1.endCoord.y - intersectY - margin, left=leftTurn, right=rightTurn, straight = goesStraight}


      _ -> Nothing
