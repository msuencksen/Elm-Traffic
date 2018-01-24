module Drawing.Lanes exposing (drawStreet, drawLaneBacklog)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

-- draw street lanes
drawStreet: Street -> List (Svg Msg)
drawStreet street =
  let
    concrete =
      case street.streetDirection of
        EastWest -> svgLane street.startCoord.x (street.startCoord.y-laneWidth) street.distance (2*laneWidth)
        NorthSouth -> svgLane (street.startCoord.x-laneWidth) street.startCoord.y (2*laneWidth) street.distance
   in
     [concrete]

-- draw single lane
svgLane: Int -> Int -> Int -> Int -> Svg Msg
svgLane px py w h =
  rect [ x (toString px), y (toString py), Svg.Attributes.width (toString w), Svg.Attributes.height (toString h), fill "#505050" ] []

-- backlog congestion display
drawLaneBacklog: Lane -> List (Svg Msg)
drawLaneBacklog lane =
  let
    fontSize =
      if lane.carBacklog < 5 then
        "16px"
      else
        if lane.carBacklog > 15 then
          "32px"
        else
          "24px"

    textPoint =
      case lane.direction of
        East -> (lane.startCoord.x+15,lane.startCoord.y + laneWidth +15)
        West -> (lane.startCoord.x+lane.distance-25,lane.startCoord.y - laneWidth)
        South ->  (lane.startCoord.x-laneWidth - 25,lane.startCoord.y + 25)
        North -> (lane.startCoord.x+laneWidth,lane.startCoord.y+lane.distance - 15)
  in
    [Svg.text_ [x (toString (Tuple.first textPoint)), y (toString (Tuple.second textPoint )),
               fill "red", Svg.Attributes.fontSize fontSize]
              [Svg.text (toString lane.carBacklog)]
    ]
