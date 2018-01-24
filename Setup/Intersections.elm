module Setup.Intersections exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

-- This module computes the intersections of all lanes.

-- Each intersection is a junction represented by a traffic light sitting on a point p of the lane (relative to lane direction).
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
          intersectX = l2.startCoord.x - l1.startCoord.x -- intersectX is relative p on Eastern lane
          intersectY = l1.startCoord.y - l2.startCoord.y -- intersectY is relative p on Southern lane
          intersecting = (intersectX >= -laneWidth && intersectX <= l1.distance + laneWidth) && (intersectY > -laneWidth && intersectY <= l2.distance + laneWidth)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight = not (abs (l1.distance - intersectX) < 2*laneWidth )
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
                Just { defaultLight | on=False, straight=goesStraight, p = intersectX - 2*margin, left=leftTurn, right=rightTurn, straight = goesStraight}


      (West,North) -> -- light for W in W+N
        let
          intersectX = l1.endCoord.x - l2.startCoord.x
          intersectY = l2.endCoord.y - l1.startCoord.y
          intersecting = (intersectX >= laneWidth && intersectX <= l1.distance + laneWidth) && (intersectY > -laneWidth && intersectY <= l2.distance + laneWidth)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight = not (abs (l1.distance - intersectX) < laneWidth )
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
                Just { defaultLight | on=False, straight=goesStraight, p = intersectX - 2 *margin, left=leftTurn, right=rightTurn}


      (South,West) -> -- light for S in SxW
        let
          intersectX = l2.endCoord.x - l1.startCoord.x
          intersectY = l2.startCoord.y - l1.startCoord.y
          intersecting = (intersectX >= -laneWidth && intersectX <= l2.distance + laneWidth) && (intersectY > laneWidth && intersectY <= l1.distance + laneWidth)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight = not (abs (l1.distance - intersectY) < 2*laneWidth )
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
          intersectY = l1.endCoord.y - l2.startCoord.y
          intersecting = (intersectX >= -laneWidth && intersectX <= l2.distance + laneWidth) && (intersectY > -laneWidth && intersectY <= l1.distance + laneWidth)
        in
          case intersecting of
            False -> Nothing
            True ->
              let
                goesStraight =  not ((abs (l1.distance - intersectY)) < 2* laneWidth )
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
                Just { defaultLight | straight=goesStraight, p = intersectY - 2*margin, left=leftTurn, right=rightTurn, straight = goesStraight}


      _ -> Nothing
