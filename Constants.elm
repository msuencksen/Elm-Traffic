module Constants exposing (..)


carLength: Int
carLength = 16

carWidth: Int
carWidth = 8

carHalfWidth: Int
carHalfWidth = carWidth // 2

carHalfLength: Int
carHalfLength = carLength // 2

carSpace: Int
carSpace = 6

carClearance: Int
carClearance = carLength+carSpace

carClearanceHalf: Int
carClearanceHalf = carClearance // 2

laneWidth: Int
laneWidth = 20

carTurnStep = 90 // (laneWidth // 2)

laneHalfWidth: Int
laneHalfWidth = laneWidth // 2

infinity: Int
infinity = 9999
