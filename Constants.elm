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

carSpeedClear: Int -- distance clear for speeding up
carSpeedClear = 3 * carClearance

laneWidth: Int
laneWidth = 20

carTurnStep = 90 // (laneWidth // 2)

laneHalfWidth: Int
laneHalfWidth = laneWidth // 2

lightWidth: Int
lightWidth = 18

lightHeight: Int
lightHeight = 32

lightSpacing: Int
lightSpacing = 3

lightFireRadius: Int
lightFireRadius = (lightWidth - 2*lightSpacing) // 2

lightFireCenterBothX: Int
lightFireCenterBothX = (lightFireRadius + lightSpacing)

lightFireCenterGreenY: Int
lightFireCenterGreenY = (3*lightFireRadius + 2*lightSpacing)

lightStreetSpacing: Int
lightStreetSpacing = laneHalfWidth

backlogLimit: Int -- limit for one lane
backlogLimit = 2

lanesOverLimit: Int -- maximum lane count over limit before "game over"
lanesOverLimit = 3


infinity: Int
infinity = 9999
