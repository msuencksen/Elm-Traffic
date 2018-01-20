module Intersections exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

-- Compute intersections of all laneStart


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
    case (l1.direction.dx, l1.direction.dy, l2.direction.dx, l2.direction.dy) of
      (1,0,0,1) ->   -- light for E in E+S
        let
          intersect = l2.startCoord.x - l1.startCoord.x
        in
          if intersect > 0 && intersect <= l1.distance then
            let
              light = { defaultLight | straight=False, p = intersect - margin, right=Just l2id, left=Just l2.oppositeLane} -- T junction
            in
              if intersect == l1.distance then
                Just light
              else
                Just { light | straight=True} -- X junction
          else
           Nothing

      (-1,0,0,-1) -> -- light for W in W+N
        let
          intersect = l1.endCoord.x - l2.startCoord.x
        in
          if intersect > 0 && intersect <= l1.distance then
            let
              light = { defaultLight | straight=False, p = intersect - margin, left=Just l2.oppositeLane, right=Just l2id} -- T junction
            in
              if intersect == l2.startCoord.x then
                Just light
              else
                Just { light | straight=True} -- X junction
          else
           Nothing

      (0,1,-1,0) -> -- light for S in SxW
        let
          intersect = l2.startCoord.y - l1.startCoord.y
        in
          if intersect > 0 && intersect <= l1.distance then
            let
              light = { defaultLight | straight=False, p = intersect - margin, left=Just l2.oppositeLane , right=Just l2id } -- T junction
            in
              if intersect == l2.startCoord.y then
                Just light
              else
                Just { light | straight=True} -- X junction
          else
           Nothing

      (0,-1,1,0) -> -- light for N in NxE
        let
          intersect = l2.startCoord.y - l1.startCoord.y
        in
          if intersect > 0 && intersect <= l1.distance then
            let
              light = { defaultLight | straight=False, p = intersect - margin, left=Just l2.oppositeLane, right=Just l2id} -- T junction
            in
              if intersect == l2.startCoord.y then
                Just light
              else
                Just { light | straight=True} -- X junction
          else
           Nothing

      _ -> Nothing
