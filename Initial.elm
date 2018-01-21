module Initial exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)

initialCar: Car
initialCar = { x=0,
 canMove = False,
 nextCarTurn=Nothing,
 turnAngle=0,
 distancePredecessor=infinity,
 carStatus = Moving }

defaultLane: Lane
defaultLane = { cars=[], direction={dx=0, dy=0}, lights=Array.empty, startCoord = {x=0,y=0}, endCoord = {x=0,y=0},
                distance = 0, oppositeLane=-1, newCarProbability=0.5, newCarRandom01=0, carBacklog =0, spawn = False }

-- initiales Model
initialModel : Model
initialModel = { lanes = -- rules for lane coordinates: every lane goes from lower start to higher end coordinates, i.e. is defined left-to-right, or top-to-bottom
               Array.fromList
               [
                -- lane 0 north E->W
                 { defaultLane |
                     direction={dx=1, dy=0},
                     startCoord = {x=0, y=420},
                     endCoord = {x=700, y=420},
                     distance = 700,
                     oppositeLane = 1,
                     newCarProbability=0.5
                 },
                 -- lane 1 north E<-W
                 { defaultLane |
                    direction={dx=-1, dy=0},
                    startCoord = {x=0, y=400},
                    endCoord = {x=700, y=400},
                    distance = 700,
                    oppositeLane = 0
                 },
                 -- lane 2 West N->S
                 { defaultLane |
                    direction={dx=0, dy=1},
                    startCoord = {x=210, y=0},
                    endCoord = {x=210, y=700},
                    distance = 700,
                    oppositeLane = 3
                 },
                 -- lane 3 West N<-S
                 { defaultLane |
                    direction={dx=0, dy=-1},
                    startCoord = {x=230, y=0},
                    endCoord = {x=230, y=700},
                    distance = 700,
                    oppositeLane = 2
                 },
                 -- lane 4 West N->S
                 { defaultLane |
                    direction={dx=0, dy=1},
                    startCoord = {x=410, y=0},
                    endCoord = {x=410, y=400},
                    distance = 400,
                    oppositeLane = 3
                 },
                 -- lane 5 West N<-S
                 { defaultLane |
                    direction={dx=0, dy=-1},
                    startCoord = {x=430, y=0},
                    endCoord = {x=430, y=400},
                    distance = 400,
                    oppositeLane = 2
                 }
               ]
               , svgLanes = []
               , pause = False
             }
