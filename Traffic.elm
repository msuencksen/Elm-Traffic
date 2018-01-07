module Traffic exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Time exposing (..)

--
-- Series9
--

initialCar: Car
initialCar = { x=0, y=100 }

carLength: Int
carLength = 16

carSpace: Int
carSpace = 2

type Direction =
  North
  | South
  | West
  | East

type alias Car = {
  x: Int, y: Int
}

type alias Street = {
  cars: List Car,
  direction: Direction
}

-- Model
type alias Model = List Car

-- updates
type Msg =
  TimerNext Time
  | CarProbability (Float,Float)


-- initiales Model
initialModel : Model
initialModel = [ initialCar ]


-- called upon start
init : (Model, Cmd a)
init =
  (initialModel, Cmd.none)

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TimerNext time -> (List.map moveCar model, Random.generate CarProbability (Random.pair (Random.float 0 1) (Random.float 0 1)) )
    CarProbability (p,p2) ->
      let
        distance =
          case (List.head model) of
            Nothing -> carLength + carSpace
            Just {x,y} -> x

        newModel =
          if p > 0.99 && distance > (carLength + carSpace) then
            initialCar :: model
          else
            model
      in
        (List.filter (\{x,y} -> (x<500)) newModel, Cmd.none)

moveCar: Car -> Car
moveCar car =
  { car | x = car.x+1 }


-- autoplay timer
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (15 * Time.millisecond) TimerNext

-- svg view
view : Model -> Html Msg
view model =
        svg [ viewBox "0 0 1000 1000", Svg.Attributes.width "1000px", Svg.Attributes.floodColor "red" ]
        (  List.map svgCar model)

-- Svg-Kreis mit Farbe zeichnen
svgCar : Car -> Svg Msg
svgCar car =
    rect [ x (toString (car.x)), y (toString (car.y)), width (toString carLength), height (toString 8), transform "scale(2,2)",fill "black" ] []



-- main program
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
