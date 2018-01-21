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
                    distance = 1600
                 },
                  { streetDirection = EastWest,
                    startCoord = {x=0, y=410},
                    distance = 1600
                  },
                  { streetDirection = EastWest,
                    startCoord = {x=0, y=210},
                    distance = 200
                  },
                  { streetDirection = EastWest,
                    startCoord = {x=0, y=610},
                    distance = 1600
                  },
                  -- northern T junction with EastWest street: y+distance must match an EastWest street with y == y + distance - laneHalfWidth
                  {  streetDirection = NorthSouth,
                     startCoord = {x=500, y=0},
                     distance = 400
                  },
                  {  streetDirection = NorthSouth,
                     startCoord = {x=800, y=0},
                     distance = 400
                  },
                  {  streetDirection = NorthSouth,
                     startCoord = {x=1400, y=0},
                     distance = 800
                  }
                  ,
                  {  streetDirection = NorthSouth,
                     startCoord = {x=500, y=600},
                     distance = 200
                  }
                  --

                ]

-- initiales Model
initialModel : Model
initialModel = { lanes = initialStreets |> Array.foldr addStreetLanes Array.empty
               , svgLanes = []
               , pause = False
             }
