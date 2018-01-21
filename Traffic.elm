module Traffic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Time exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)
import LaneSwitch exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Initial exposing (..)
import Intersections exposing (..)
import Lights exposing (..)
import TrafficDrawing exposing (..)

--
-- Elm Traffic
--

-- called upon start
init : (Model, Cmd a)
init =
  ( initialModel |> initialSetup , Cmd.none)

initialSetup : Model -> Model
initialSetup model =
  { model |
     lanes = model.lanes
             |> createLights
             |> Array.map createSpawnFlag
             |> findSwitchClusters

     , svgLanes =
       model.lanes
       |> Array.map drawLaneElements |> Array.foldr (++) []
  }

createSpawnFlag : Lane -> Lane
createSpawnFlag lane =
  { lane | spawn = (lane.startCoord.x==0 && lane.direction == East)
                    || (lane.endCoord.x == cityMapWidth && lane.direction == West)
                    || (lane.startCoord.y==0 && lane.direction == South)
                    || (lane.endCoord.y== cityMapHeight && lane.direction == North)
  }

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pause -> ( {model | pause = not model.pause }, Cmd.none)
    Reset -> init
    TimerNext time ->
      if not model.pause then
         (updateModel model, Cmd.batch (randomNumbers model) )
      else
        (model, Cmd.none)
    SwitchLight laneNo lightsNo ->
      ( { model | lanes = switchLights model.lanes laneNo lightsNo }, Cmd.none)

    CarProbability randomList ->
      let
        randomArray = Array.fromList randomList
        laneZippedWithRandom = Array.Extra.map2 (,) model.lanes randomArray
      in
        ({ model | lanes = Array.map addNewCar laneZippedWithRandom }, Cmd.none)

    TurnProbability laneId randomFloats ->
      ({model | lanes = updateTurns laneId model.lanes (Array.fromList randomFloats)}, Cmd.none)

updateTurns: Int -> Array Lane -> Array Float -> Array Lane
updateTurns laneId lanes randomFloats =
  let
    getLane = Array.get laneId lanes
  in
    case getLane of
      Nothing -> lanes
      Just lane ->
        let
          lightsZippedWithRandom = Array.Extra.map2 (,) lane.lights randomFloats
        in
          lanes |> Array.set laneId { lane | lights = Array.Extra.map2 probeNextTurn lane.lights randomFloats}

probeNextTurn: Light -> Float -> Light
probeNextTurn light randomFloat =
  { light | nextCarTurn =
    case (light.left, light.right, light.straight) of
      (Nothing, Nothing, False) -> Nothing
      (Nothing, Nothing, True) -> Just Straight
      (Just l, Nothing, False) -> Just (Left l)
      (Just l, Nothing, True) -> if (randomFloat < 0.3) then Just (Left l) else Just Straight
      (Nothing, Just r, False) -> Just (Right r)
      (Nothing, Just r, True) -> if (randomFloat < 0.3) then Just (Right r) else Just Straight
      (Just l, Just r, False) -> if (randomFloat < 0.5) then Just (Left l) else Just (Right r)
      (Just l, Just r, True) -> if (randomFloat < 0.33) then Just (Left l) else if (randomFloat > 0.66) then Just (Right r) else Just Straight
  }

randomNumbers: Model -> List (Cmd Msg)
randomNumbers model =
  let
    -- probability for new car in lane
    laneRandomCar = Random.generate CarProbability (Random.list (Array.length model.lanes) (Random.float 0 1))

    -- probability for turns at junction lights
    laneRandomTurns = Array.toList (Array.indexedMap (\laneId lane -> (Random.generate (TurnProbability laneId) (Random.list (Array.length lane.lights) (Random.float 0 1)) ) ) model.lanes)

  in
    laneRandomCar :: laneRandomTurns

-- add possible new car, update backlog
addNewCar: (Lane,Float) -> Lane
addNewCar (lane,probability) =
  let
    distance =
      case (List.head lane.cars) of
        Nothing -> infinity
        Just car -> car.x
    carToAdd =
      if lane.spawn && probability > 0.991 then
        1
      else
        0
  in
    if (carToAdd > 0 || lane.carBacklog > 0) then
      if distance < carHalfLength + carSpace then
         { lane | carBacklog = lane.carBacklog + carToAdd } -- no free space for car
      else
        { lane | cars = initialCar :: lane.cars,
                 carBacklog = lane.carBacklog + carToAdd - 1} -- get from backlog
    else
      lane -- nothing to do

switchLights: Array Lane -> Int -> Int -> Array Lane
switchLights allLanes laneNo lightsNo =
  case Array.get laneNo allLanes of
    Nothing -> allLanes
    Just lane ->
      case Array.get lightsNo lane.lights of
        Nothing -> allLanes
        Just light ->
          let
            newLightState = (not light.on) -- toggles off/on
            thisLightIndex = { laneId=laneNo, lightId = lightsNo}
          in
            allLanes |> setLightAtIndex (Just thisLightIndex) newLightState
                     |> setLightAtIndex light.oppositeLightIndex newLightState
                     |> setLightAtIndex light.leftLightIndex (not newLightState)
                     |> setLightAtIndex light.rightLightIndex (not newLightState)



-- toggle light defined by LightIndex
setLightAtIndex: Maybe LightIndex -> Bool -> Array Lane -> Array Lane
setLightAtIndex maybeLightIndex lightState allLanes =
  case maybeLightIndex of
    Nothing -> allLanes
    Just lightIndex ->
      case Array.get lightIndex.laneId allLanes of
        Nothing -> allLanes
        Just lane ->
          allLanes |> Array.set lightIndex.laneId (switchLightNo lane lightIndex.lightId lightState) -- update lane with switched light state

-- toggle light's .on field
switchLightNo: Lane -> Int -> Bool -> Lane
switchLightNo lane lightsNo state =
  case Array.get lightsNo lane.lights of
    Nothing -> lane
    Just light -> {lane | lights = Array.set lightsNo { light | on = state } lane.lights }
    --Just light -> {lane | lights = Array.set lightsNo { light | on = not light.on } lane.lights } -- toggles light.on


updateModel: Model -> Model
updateModel model =
  let
    turningCars = List.filter (\car -> car.nextCarTurn /= Nothing && car.nextCarTurn /= Just Straight)
    -- carsToLane = turningCars |> List.map (\car ->
    --   case car.nextCarTurn of
    --     Nothing -> (car, -1)
    --     Just Straight -> (car, -1)
    --     Just (Left l) -> (car, l)
    --     Just (Right r) -> (car, r)
    -- )

    laneUpdate = model.lanes |> Array.map checkCarMovement |> processCarLaneSwitch |> Array.map processCarMove
  in
    {model | lanes = laneUpdate}


-- check car movement:
-- Going through lane's car list with foldr,
-- since cars most advanced are at the end of the list.
checkCarMovement: Lane -> Lane
checkCarMovement lane =
  { lane | cars = lane.cars |> List.foldr (checkCarMove lane.direction lane.lights) [] }

-- Function for use with foldr:
-- current car is checked for
-- a. distance from first car of current cars list
-- b.
checkCarMove: Direction -> Lights -> Car -> List Car -> List Car
checkCarMove direction lights car cars =
  let
    carInFront = List.head cars

    abstandCar =
      case carInFront of
        Nothing -> infinity
        Just carInFront ->
          carInFront.x - car.x

    nextTrafficLight =
      lights |> Array.foldl (\light nextLight -> nearestLight car.x light nextLight) Nothing

    carClear1 = (abstandCar > carClearance)

    carLightsDistance =
        case nextTrafficLight of
          Nothing -> infinity
          Just light -> light.p - car.x

    carClearLights1 = carLightsDistance > carClearanceHalf -- still away from lights stop
    carClearLights2 = carLightsDistance < 0 -- passed nearest light
    carClearLights3 = Maybe.withDefault False (Maybe.map (\light -> not light.on) nextTrafficLight) -- lights are green

    lightsClear = (carClearLights1 || carClearLights2 || carClearLights3)

    junctionJammed =
      case carInFront of
        Nothing -> False
        Just carx -> not carClearLights2 && not carClearLights1 && carClearLights3 && (abstandCar <=  carSpace + 2*laneWidth) -- && carx.carStatus /= Moving

    -- get a turn decision from random number stored with light
    carTurn =
      if (carLightsDistance == carClearanceHalf && car.nextCarTurn == Nothing ) then
        case nextTrafficLight of
          Nothing -> Nothing
          Just light ->
            case light.nextCarTurn of
              Nothing -> Nothing
              Just carTurn -> Just carTurn
      else
        car.nextCarTurn

    carCanMove = -- bool
      carClear1 && lightsClear && not junctionJammed

    carTurnAngle =
      case car.nextCarTurn of
        Nothing -> 0
        Just Straight -> 0
        Just (Left _) ->
          -- for left turns, move over 1.5 * lane width
          if carLightsDistance < -laneWidth && carLightsDistance >= -( laneWidth + laneHalfWidth) then
            car.turnAngle - carTurnStep
          else
            car.turnAngle
        Just (Right _) ->
          -- for left turns, move over 0.5 * lane width
          if carLightsDistance < 0 && carLightsDistance >= -laneHalfWidth then
            car.turnAngle + carTurnStep
          else
            car.turnAngle

    isTurning = not (carTurn ==  Nothing || carTurn == Just Straight)

    movedCar =
      { car |
         distancePredecessor = abstandCar,
         canMove = carCanMove && not isTurning,
         nextCarTurn = carTurn,
         carStatus =
           if not isTurning then
             case (carCanMove, lightsClear) of
               (False, False) -> LightsStop
               (True, False) -> LightsStop
               (False, True) -> JamStop
               (True, True) -> Moving
             else
               if lightsClear then
                 Turning
               else
                 LightsStop
                 ,
         turnAngle =
          if car.turnAngle > 0 then
            car.turnAngle - carTurnStep
         else
           if car.turnAngle < 0 then
             car.turnAngle + carTurnStep
           else
             car.turnAngle

      }
  in
    movedCar :: cars

nearestLight: Int -> Light -> Maybe Light -> Maybe Light
nearestLight cx light1 light2 =
  case (light1, light2) of
    (l1, Nothing) -> Just l1
    (l1, Just l2) ->
      if abs (l1.p-cx) < abs (l2.p-cx) then
        Just l1
      else
        Just l2


processCarMove: Lane -> Lane
processCarMove lane =
  { lane | cars = lane.cars |> List.map moveCar |> List.filter (\car -> car.x < lane.distance) }

moveCar: Car -> Car
moveCar car =
  case car.canMove of
    False -> car
    True -> { car |
              x = car.x+1,
              nextCarTurn = Nothing,
              carStatus = Moving
            }



-- autoplay timer
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (15 * Time.millisecond) TimerNext

-- svg view
view : Model -> Html Msg
view model =

        div [bodyStyle]
        [ h4 [] [Html.text "ElmTown 9e"]
        , button [ onClick (SwitchLight 0 0) ] [ Html.text "LightsW>E" ]
        , button [ onClick (SwitchLight 1 0) ] [ Html.text "LightsE>W" ]
        , button [ onClick (SwitchLight 2 0) ] [ Html.text "LightsN>S" ]
        , button [ onClick (SwitchLight 3 0) ] [ Html.text "LightsS>N" ]
        , button [ onClick Reset ] [ Html.text "Reset" ]
        , checkbox Pause "Pause"
        , br [] []

        , div [svgBoxStyle] [
            div [lawnStyle] [],
            div [svgStyle]
            [
              svg [ viewBox ("0 0 "++(Basics.toString cityMapWidth)++" "++ (Basics.toString cityMapHeight)), Svg.Attributes.width ((toString cityMapWidth)++"px"),  Svg.Attributes.height ((toString cityMapHeight)++"px")]
              (
              model.svgLanes -- streets
              ++ (Array.toList (Array.indexedMap drawLightElements model.lanes) |> List.foldr (++) []) -- lights
              ++ (model.lanes |> Array.filter (\lane -> lane.spawn) |> Array.map drawLaneBacklog |> Array.foldr (++) []) --
              ++
              (  model.lanes |> Array.map (\lane -> List.map (svgCarBox lane) lane.cars  |> List.foldr (++) [] ) |> Array.foldr (++) [] )-- flatmap
              )
            ]
          ]

        ]



-- html checkbox
checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [] [ input [ Html.Attributes.type_ "checkbox", onClick msg ] [], Html.text title ]


bodyStyle =
   Html.Attributes.style [("backgroundColor", "black"), ("color","white"), ("width","100%"),("height","100%")]

lawnStyle =
   Html.Attributes.style [("position","absolute"),("backgroundColor", "green"),("width", "200px"), ("height","400px")]

svgBoxStyle =
  Html.Attributes.style [("margin","auto"),("width",(toString cityMapWidth)++"px"),("height",(toString cityMapHeight)++"px"),("position","relative"),("top","20px"),("backgroundColor", "orange"),("border","1px"),("border-color","white")]

svgStyle =
  Html.Attributes.style [("position","absolute")]


-- main program
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
