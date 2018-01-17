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

-- initiales Model
initialModel : Model
initialModel = { lanes = -- rules for lane coordinates: every lane goes from lower start to higher end coordinates, i.e. is defined left-to-right, or top-to-bottom
               Array.fromList
               [
                -- lane 0 north E->W
                 { cars=[],
                   direction={dx=1, dy=0},
                   lights=Array.empty, -- Array.fromList[{on=True, p=210-laneHalfWidth, left=Just 3, right=Just 2, straight=True, nextCarTurn=Nothing}],
                   startCoord = {x=0, y=360},
                   endCoord = {x=700, y=360},
                   distance = 700,
                   oppositeLane = 1,
                   newCarProbability=0.5,
                   newCarRandom01=0,
                   carBacklog = 0
                 },
                 -- lane 1 north E<-W
                 { cars=[],
                   direction={dx=-1, dy=0},
                  lights=Array.empty, -- Array.fromList [{on=True, p=470-laneHalfWidth, left=Just 2, right=Just 3, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=0, y=340},
                  endCoord = {x=700, y=340},
                  distance = 700,
                  oppositeLane = 0,
                  newCarProbability=0.5,
                  newCarRandom01=0,
                  carBacklog = 0
                 },
                 -- lane 2 West N->S
                 { cars=[],
                   direction={dx=0, dy=1},
                  lights=Array.empty, -- Array.fromList [{on=True, p=340-laneHalfWidth, left=Just 0, right=Just 1, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=210, y=0},
                  endCoord = {x=210, y=700},
                  distance = 700,
                  oppositeLane = 3,
                  newCarProbability=0.5,
                  newCarRandom01=0,
                  carBacklog = 0
                 },
                 -- lane 3 West N<-S
                 { cars=[],
                   direction={dx=0, dy=-1},
                  lights=Array.empty, -- Array.fromList [{on=True, p=338, left=Just 1, right=Just 0, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=230, y=0},
                  endCoord = {x=230, y=700},
                  distance = 700,
                  oppositeLane = 2,
                  newCarProbability=0.5,
                  newCarRandom01=0,
                  carBacklog = 0
                 },
                 -- lane 4 West N->S
                 { cars=[],
                   direction={dx=0, dy=1},
                  lights=Array.empty, -- Array.fromList [{on=True, p=340-laneHalfWidth, left=Just 0, right=Just 1, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=410, y=0},
                  endCoord = {x=410, y=340},
                  distance = 340,
                  oppositeLane = 3,
                  newCarProbability=0.5,
                  newCarRandom01=0,
                  carBacklog = 0
                 },
                 -- lane 5 West N<-S
                 { cars=[],
                   direction={dx=0, dy=-1},
                  lights=Array.empty, -- Array.fromList [{on=True, p=338, left=Just 1, right=Just 0, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=430, y=0},
                  endCoord = {x=430, y=340},
                  distance = 340,
                  oppositeLane = 2,
                  newCarProbability=0.5,
                  newCarRandom01=0,
                  carBacklog = 0
                 }
               ]
               , svgLanes = []
               , pause = False
             }
