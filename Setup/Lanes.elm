module Setup.Lanes exposing (addStreetLanes,createSpawnFlag)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)

-- This modules generates the lane array from the street array.

defaultLane: Lane
defaultLane = { cars=[], direction=East, lights=Array.empty, startCoord = {x=0,y=0}, endCoord = {x=0,y=0},
                distance = 0, oppositeLane=-1, newCarProbability=0.5, newCarRandom01=0, carBacklog =0, spawn = False }

-- For one street, push two opposite lanes to the lanes array.
addStreetLanes: Street -> Array Lane -> Array Lane
addStreetLanes street lanes =
  let
    nextLaneId = Array.length lanes
    oppositeLaneId = nextLaneId + 1

    laneDirection =
      case street.streetDirection of
        EastWest -> East
        NorthSouth -> South
    oppositeLaneDirection =
      case street.streetDirection of
        EastWest -> West
        NorthSouth -> North

    firstNewLane = { defaultLane |
        direction = laneDirection,
        startCoord = getLaneStartCoord street laneDirection,
        endCoord = getLaneEndCoord street laneDirection,
        distance = street.distance,
        oppositeLane = oppositeLaneId -- id
    }

    secondNewLane = { defaultLane |
        direction = oppositeLaneDirection,
        startCoord = getLaneStartCoord street oppositeLaneDirection,
        endCoord = getLaneEndCoord street oppositeLaneDirection,
        distance = street.distance,
        oppositeLane = nextLaneId -- id
    }

  in
    lanes |> Array.push firstNewLane |> Array.push secondNewLane

-- get lane starting point for a lane direction
getLaneStartCoord: Street -> Direction -> Point
getLaneStartCoord street laneDirection =
  case laneDirection of
    East -> { x = street.startCoord.x, y = street.startCoord.y + laneHalfWidth } -- lower lane
    West -> { x = street.startCoord.x, y = street.startCoord.y - laneHalfWidth } -- upper
    South -> { x = street.startCoord.x - laneHalfWidth, y = street.startCoord.y } -- left lane
    North -> { x = street.startCoord.x + laneHalfWidth, y = street.startCoord.y } -- right

-- get lane ending point for a lane direction
getLaneEndCoord: Street -> Direction -> Point
getLaneEndCoord street laneDirection =
  case laneDirection of
    East -> { x = street.startCoord.x + street.distance, y = street.startCoord.y + laneHalfWidth }  -- lower lane
    West -> { x = street.startCoord.x + street.distance, y = street.startCoord.y - laneHalfWidth } -- upper
    South -> { x = street.startCoord.x - laneHalfWidth, y = street.startCoord.y + street.distance } -- left lane
    North -> { x = street.startCoord.x + laneHalfWidth, y = street.startCoord.y + street.distance } -- right

-- spawn flag: only lane starting at the map edges will spawn new cars
createSpawnFlag : Lane -> Lane
createSpawnFlag lane =
  { lane | spawn = (lane.startCoord.x == 0 && lane.direction == East)
                    || (lane.endCoord.x == cityMapWidth && lane.direction == West)
                    || (lane.startCoord.y == 0 && lane.direction == South)
                    || (lane.endCoord.y == cityMapHeight && lane.direction == North)
  }
