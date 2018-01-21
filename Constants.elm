module Constants exposing (..)

cityMapWidth: Int
cityMapWidth = 1600

cityMapHeight: Int
cityMapHeight = 800


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

lightWidth: Int
lightWidth = 14

lightHeight: Int
lightHeight = 30

lightSpacing: Int
lightSpacing = 2

lightFireRadius: Int
lightFireRadius = (lightWidth - 2*lightSpacing) // 2

lightFireCenterBothX: Int
lightFireCenterBothX = (lightFireRadius + lightSpacing)

lightFireCenterGreenY: Int
lightFireCenterGreenY = (3*lightFireRadius + 2*lightSpacing)

lightStreetSpacing = laneHalfWidth

infinity: Int
infinity = 9999
