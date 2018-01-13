module Series9 exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Time exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)
--
-- Series9
--


carLength: Int
carLength = 16

carWidth: Int
carWidth = 8

carHalfWidth: Int
carHalfWidth = carWidth // 2

carHalfLength: Int
carHalfLength = carLength // 2

carSpace: Int
carSpace = 2

carClearance: Int
carClearance = carLength+carSpace

laneWidth: Int
laneWidth = 20

carTurnStep = 90 // (laneWidth // 2)

laneHalfWidth: Int
laneHalfWidth = laneWidth // 2

infinity: Int
infinity = 9999

type alias Direction = { dx: Int, dy: Int }

type CarTurn =
  Left Int | Right Int | Straight

type alias Car = {
  x: Int,
  nextCarTurn: Maybe CarTurn,
  turnAngle: Int
}

initialCar: Car
initialCar = { x=0, nextCarTurn=Nothing, turnAngle=0 }

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

-- initiales Model
initialModel : Model
initialModel = { lanes = Array.fromList
               [
                -- lane 0
                 { cars=[initialCar],
                   direction={dx=1, dy=0},
                   lights=Array.fromList[{on=True, p=200, left=Just 3, right=Just 2, straight=True, nextCarTurn=Nothing}],
                   startCoord = {x=0, y=112},
                   endCoord = {x=1000, y=112},
                   newCarProbability=0.5,
                   newCarRandom01=0
                 },
                 -- lane 1
                 { cars=[initialCar, initialCar],
                   direction={dx=-1, dy=0},
                  lights=Array.fromList [{on=True, p=780, left=Just 2, right=Just 3, straight=True, nextCarTurn=Nothing}, {on=True, p=220, left=Just 2, right=Just 3, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=0, y=100},
                  endCoord = {x=1000, y=100},
                  newCarProbability=0.5,
                  newCarRandom01=0
                 },
                 -- lane 2
                 { cars=[initialCar],
                   direction={dx=0, dy=1},
                  lights=Array.fromList [{on=True, p=90, left=Just 0, right=Just 1, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=210, y=00},
                  endCoord = {x=210, y=700},
                  newCarProbability=0.5,
                  newCarRandom01=0
                 },
                 -- lane 3
                 { cars=[initialCar],
                   direction={dx=0, dy=-1},
                  lights=Array.fromList [{on=True, p=590, left=Just 1, right=Just 0, straight=True, nextCarTurn=Nothing}],
                  startCoord = {x=222, y=0},
                  endCoord = {x=222, y=700},
                  newCarProbability=0.5,
                  newCarRandom01=0
                 }
               ]
               , svgLanes = []
             }

-- called upon start
init : (Model, Cmd a)
init =
  ( {initialModel | svgLanes = Array.map drawLaneElements initialModel.lanes |> Array.foldr (++) []}, Cmd.none)

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pause -> (model, Cmd.none)
    Reset -> init
    TimerNext time -> (updateModel model, Cmd.batch (randomNumbers model) )
    SwitchLight laneNo lightsNo ->
      case Array.get laneNo model.lanes of
        Nothing -> (model, Cmd.none)
        Just lane -> ( { model | lanes = Array.set laneNo (switchLightNo lane lightsNo) model.lanes}, Cmd.none)

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
      (Just l, Just r, True) -> if (randomFloat < 0.25) then Just (Left l) else if (randomFloat > 0.75) then Just (Right r) else Just Straight
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

addNewCar: (Lane,Float) -> Lane
addNewCar (lane,probability) =
  let
    distance =
      case (List.head lane.cars) of
        Nothing -> carLength + carSpace
        Just car -> car.x
  in
    if probability > 0.99 && distance > (carLength + carSpace) then
      { lane | cars = initialCar :: lane.cars}
    else
      lane


switchLightNo: Lane -> Int -> Lane
switchLightNo lane lightsNo =
  case Array.get lightsNo lane.lights of
    Nothing -> lane
    Just light -> {lane | lights = Array.set lightsNo { light | on = not light.on } lane.lights } -- toggles light.on


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


    laneUpdate = Array.map processLane model.lanes
  in
    {model | lanes = laneUpdate}

processLane: Lane -> Lane
processLane lane =
  { lane | cars = List.foldr (moveCar lane.direction lane.lights) [] lane.cars }

moveCar: Direction -> Lights -> Car -> List Car -> List Car
moveCar direction lights car cars =
  let
    abstandCar =
      case cars of
        [] -> infinity
        firstCar :: carList ->
          firstCar.x-car.x

    nextTrafficLight =
      lights |> Array.foldl (\light nextLight -> nearestLight car.x light nextLight) Nothing

    carClear1 = (abstandCar > carClearance)

    carLightsDistance =
        case nextTrafficLight of
          Nothing -> infinity
          Just light -> light.p - car.x

    carMoveX =
      if carClear1 && ( carLightsDistance > carClearance) || carLightsDistance < 0 || Maybe.withDefault False (Maybe.map (\light -> not light.on) nextTrafficLight) then
        1
      else
        0

    -- get a turn decision from random number stored with light
    carTurn =
      if (car.nextCarTurn == Nothing) then
        if (carLightsDistance == carClearance) then
          case nextTrafficLight of
            Nothing -> Nothing
            Just light ->
              case light.nextCarTurn of
                Nothing -> Nothing
                Just carTurn -> Just carTurn
        else
          Nothing
      else
        car.nextCarTurn

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

    movedCar =
      { car |
         x = car.x + carMoveX,
         nextCarTurn = carTurn
         ,turnAngle =
          if carMoveX /= 0 then
            carTurnAngle
         else
           0
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


-- autoplay timer
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (15 * Time.millisecond) TimerNext

-- svg view
view : Model -> Html Msg
view model =

        div [bodyStyle]
        [ h4 [] [Html.text "ElmTown 9e"]
        , button [ onClick (SwitchLight 0 0) ] [ Html.text "Lights1" ]
        , button [ onClick (SwitchLight 1 0) ] [ Html.text "Lights2" ]
        , button [ onClick (SwitchLight 1 1) ] [ Html.text "Lights3" ]
        , button [ onClick Reset ] [ Html.text "Reset" ]
        , checkbox Pause "Pause"
        , br [] []



        , div [svgBoxStyle] [
            div [lawnStyle] [],
            div [svgStyle]
            [
              svg [ viewBox "0 0 1000 700", Svg.Attributes.width "1000px" ]
              (
              model.svgLanes ++
              -- Array.map (\lane -> List.map (svgCar lane) lane.cars) model |> Array.foldr (++) [] -- flatmap
              (Array.map (\lane -> List.map (svgCarBox lane) lane.cars) model.lanes |> Array.foldr (++) []) -- flatmap

              )
            ]
          ]

        ]

drawLaneElements: Lane -> List (Svg Msg)
drawLaneElements lane =
  let
    laneConcrete =
      if lane.direction.dx /= 0 then
        svgLane lane.startCoord.x (lane.startCoord.y-laneHalfWidth) (lane.endCoord.x - lane.startCoord.x) laneWidth
      else
        svgLane (lane.startCoord.x-laneHalfWidth) (lane.startCoord.y) laneWidth (lane.endCoord.y - lane.startCoord.y)

  in
    [laneConcrete]

-- svgLane
svgLane: Int -> Int -> Int -> Int -> Svg Msg
svgLane px py w h =
  rect [ x (toString px), y (toString py), Svg.Attributes.width (toString w), Svg.Attributes.height (toString h), fill "black" ] []

svgCarBox : Lane -> Car -> Svg Msg
svgCarBox lane car =
  let
    px =
      case lane.direction.dx of
        (1) -> car.x - carHalfLength -- car position on lane going east
        (-1) -> lane.endCoord.x - car.x + carHalfLength -- car position on lane going west
        _ -> lane.startCoord.x - carHalfWidth + (turnDeltaCar car lane.direction.dy) -- fixed horizontal position for lane going south or north

    py =
      case lane.direction.dy of
        (1) -> car.x - carHalfLength -- position on lane going south
        (-1) -> lane.endCoord.y - car.x + carHalfLength -- position on lane going north
        _ -> lane.startCoord.y - carHalfWidth + (turnDeltaCar car lane.direction.dx) -- fixed vertical position for lane going south or north

    boxWidth =
      if lane.direction.dx /= 0 then
        carLength
      else
        carWidth

    boxHeight =
      if lane.direction.dx /= 0 then
        carWidth
      else
        carLength
  in
    svgCar px py boxWidth boxHeight car.turnAngle (svgCarColor car.nextCarTurn)

turnDeltaCar: Car -> Int -> Int
turnDeltaCar car direction =
  if car.turnAngle < 0 then
    (car.turnAngle // carTurnStep) * direction
  else
    (car.turnAngle // carTurnStep) * direction

-- Svg Car
svgCar : Int -> Int -> Int -> Int -> Int -> String -> Svg Msg
svgCar px py w h rotationAngle carColorStr =
    rect [ x (toString px),
           y (toString py),
           Svg.Attributes.width (toString w),
           Svg.Attributes.height (toString h),
           transform ("rotate("++ (toString rotationAngle) ++"," ++ (toString (px+ carHalfLength)) ++ "," ++ (toString (py+carHalfWidth)) ++")" ),
           fill carColorStr ] []

svgCarColor: Maybe CarTurn -> String
svgCarColor maybeCarTurn =
  case maybeCarTurn of
    Nothing -> "grey"
    Just carTurn ->
      case carTurn of
        Left _ -> "green"
        Right _ -> "red"
        Straight -> "white"


-- html checkbox
checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [] [ input [ Html.Attributes.type_ "checkbox", onClick msg ] [], Html.text title ]


bodyStyle =
   Html.Attributes.style [("backgroundColor", "black"), ("color","white"), ("width","100%"),("height","100%")]

lawnStyle =
   Html.Attributes.style [("position","absolute"),("backgroundColor", "green"),("width", "200px"), ("height","400px")]

svgBoxStyle =
  Html.Attributes.style [("margin","auto"),("width","1000px"),("height","700px"),("position","relative"),("top","100px"),("backgroundColor", "orange"),("border","1px"),("border-color","white")]

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
