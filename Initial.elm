module Initial exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)
import Setup exposing (..)

initialCar: Car
initialCar = {
 x=0,
 canMove = False,
 nextCarTurn=Nothing,
 turnAngle=0,
 distancePredecessor=infinity,
 carStatus = Moving }



initialStreets: Array Street
initialStreets = Array.fromList
                [
                 --
                 {  streetDirection = NorthSouth,
                    startCoord = {x=210, y=0},
                    distance = 700
                 },
                  { streetDirection = EastWest,
                    startCoord = {x=0, y=410},
                    distance = 700
                  },
                  -- northern T junction with EastWest street: y+distance must match an EastWest street with y == y + distance - laneHalfWidth
                  {  streetDirection = NorthSouth,
                     startCoord = {x=400, y=0},
                     distance = 400
                  }
                  --

                ]

-- initiales Model
initialModel : Model
initialModel = { lanes = initialStreets |> Array.foldr addStreetLanes Array.empty
               , svgLanes = []
               , pause = False
             }
