module Types exposing (..)
import Constants exposing (..)
import Array exposing (..)
import Svg exposing (..)
import Time exposing (..)

type alias Direction = { dx: Int, dy: Int }

type CarTurn =
  Left Int | Right Int | Straight

type TurnTo =
  TurnToLeft | TurnToRight

type CarStatus =
  Moving | LightsStop | JamStop | WaitLeftTurn | Turning | TurningIn

type alias Car = {
  x: Int,
  distancePredecessor: Int,
  canMove: Bool,
  nextCarTurn: Maybe CarTurn,
  turnAngle: Int,
  carStatus: CarStatus
}

type alias Point = {x: Int, y: Int}

type alias Light = {
  on: Bool,
  p: Int,
  left: Maybe Int,
  right: Maybe Int,
  straight: Bool,
  nextCarTurn: Maybe CarTurn, -- populate randomly
  leftLightIndex: Maybe LightIndex,
  rightLightIndex: Maybe LightIndex,
  oppositeLightIndex: Maybe LightIndex
}

type LightFire =
  Red Bool | Green Bool

type alias Lights =
  Array Light

type alias Lane = {
  cars: List Car,
  direction: Direction,
  lights: Lights,
  startCoord: Point,
  endCoord: Point,
  distance: Int,
  oppositeLane: Int,
  newCarProbability: Float,
  newCarRandom01: Float,
  carBacklog: Int,
  spawn: Bool
}

type alias LightIndex = {
  laneId : Int,
  lightId : Int
}

--
-- Model
--
type alias Model = {
  lanes : Array Lane,
  svgLanes : List (Svg Msg),
  pause: Bool
  }

-- updates
type Msg =
  TimerNext Time
  | SwitchLight Int Int
  | CarProbability (List Float)
  | TurnProbability Int (List Float)
  | Reset
  | Pause
