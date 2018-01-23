module Interaction.User exposing (switchLights)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)

-- switch traffic lights at junction
switchLights: Array Lane -> Int -> Int -> Array Lane
switchLights allLanes laneNo lightsNo =
  case Array.get laneNo allLanes of
    Nothing -> allLanes
    Just lane ->
      case Array.get lightsNo lane.lights of
        Nothing -> allLanes
        Just light ->
          let
            newLightState = (not light.on) -- toggles off/on
            thisLightIndex = { laneId=laneNo, lightId = lightsNo}
          in
            allLanes |> setLightAtIndex (Just thisLightIndex) newLightState
                     |> setLightAtIndex light.oppositeLightIndex newLightState
                     |> setLightAtIndex light.leftLightIndex (not newLightState)
                     |> setLightAtIndex light.rightLightIndex (not newLightState)



-- toggle light defined by LightIndex
setLightAtIndex: Maybe LightIndex -> Bool -> Array Lane -> Array Lane
setLightAtIndex maybeLightIndex lightState allLanes =
  case maybeLightIndex of
    Nothing -> allLanes
    Just lightIndex ->
      case Array.get lightIndex.laneId allLanes of
        Nothing -> allLanes
        Just lane ->
          allLanes |> Array.set lightIndex.laneId (switchLightNo lane lightIndex.lightId lightState) -- update lane with switched light state

-- toggle light's .on field
switchLightNo: Lane -> Int -> Bool -> Lane
switchLightNo lane lightsNo state =
  case Array.get lightsNo lane.lights of
    Nothing -> lane
    Just light -> {lane | lights = Array.set lightsNo { light | on = state } lane.lights }
    --Just light -> {lane | lights = Array.set lightsNo { light | on = not light.on } lane.lights } -- toggles light.on
