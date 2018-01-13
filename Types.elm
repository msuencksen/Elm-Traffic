module Types exposing (..)
import Constants exposing (..)
import Array exposing (..)
import Svg exposing (..)
import Time exposing (..)

type alias Direction = { dx: Int, dy: Int }

type CarTurn =
  Left Int | Right Int | Straight

type alias Car = {
  x: Int,
  canMove: Bool,
  nextCarTurn: Maybe CarTurn,
  turnAngle: Int
}

initialCar: Car
initialCar = { x=0, canMove = False, nextCarTurn=Nothing, turnAngle=0 }

type alias Point = {x: Int, y: Int}

type alias Light = {
  on: Bool,
  p: Int,
  left: Maybe Int,
  right: Maybe Int,
  straight: Bool,
  nextCarTurn: Maybe CarTurn
}

type alias Lights =
  Array Light

type alias Lane = {
  cars: List Car,
  direction: Direction,
  lights: Lights,
  startCoord: Point,
  endCoord: Point,
  distance: Int,
  newCarProbability: Float,
  newCarRandom01: Float
}

-- Model
type alias Model = {
  lanes : Array Lane,
  svgLanes : List (Svg Msg)
  }

-- updates
type Msg =
  TimerNext Time
  | SwitchLight Int Int
  | CarProbability (List Float)
  | TurnProbability Int (List Float)
  | Reset
  | Pause
