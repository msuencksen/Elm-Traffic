module Initial exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)

-- initiales Model
initialModel : Model
initialModel = { lanes = Array.fromList
               [
                -- lane 0 north E->W
                 { cars=[initialCar],
                   direction={dx=1, dy=0},
                   lights=Array.fromList[{on=True, p=200, left=Just 3, right=Just 2, straight=True, nextCarTurn=Nothing}],
                   startCoord = {x=0, y=360},
                   endCoord = {x=700, y=360},
                   distance = 700,
                   newCarProbability=0.5,
                   newCarRandom01=0
                 },
                 -- lane 1 north E<-W
                 { cars=[initialCar, initialCar],
                   direction={dx=-1, dy=0},
                  lights=Array.fromList [{on=True, p=480, left=Just 2, right=Just 3, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=0, y=340},
                  endCoord = {x=700, y=340},
                  distance = 700,
                  newCarProbability=0.5,
                  newCarRandom01=0
                 },
                 -- lane 2 West N->S
                 { cars=[initialCar],
                   direction={dx=0, dy=1},
                  lights=Array.fromList [{on=True, p=342, left=Just 0, right=Just 1, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=210, y=0},
                  endCoord = {x=210, y=700},
                  distance = 700,
                  newCarProbability=0.5,
                  newCarRandom01=0
                 },
                 -- lane 3 West N<-S
                 { cars=[initialCar],
                   direction={dx=0, dy=-1},
                  lights=Array.fromList [{on=True, p=338, left=Just 1, right=Just 0, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=230, y=0},
                  endCoord = {x=230, y=700},
                  distance = 700,
                  newCarProbability=0.5,
                  newCarRandom01=0
                 }
               ]
               , svgLanes = []
             }
