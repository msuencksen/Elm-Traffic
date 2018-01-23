module Traffic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)
import Array exposing (..)
import Array.Extra exposing (..)
import Types exposing (..)
import Constants exposing (..)
import Initial exposing (..)
import Logic.Cars exposing (..)
import Logic.LaneSwitch exposing (..)
import Logic.Game exposing (..)
import Interaction.RandomEffects exposing (..)
import Interaction.User exposing (..)
import Drawing.Lanes exposing (..)
import Drawing.Lights exposing (..)
import Drawing.Cars exposing (..)
--
-- --- Elm Traffic ---
--
-- Author: Matthias Süncksen
--
-- Goal of the game: switch the traffic lights to keep the cars flowing.
--
-- The basic data structure is an <Array Lane> with each Lane having
-- an <Array Light> for every intersection and a <List Car> (of course).
--
-- The traffic lights are generated during initialization.
-- Spawning of new cars and car turning decisions are random effects.

-- For street map setup, see Initial.elm and Setup/*.elm
--
-- Random numbers: Interaction/RandomEffects.elm
-- Car movement: Logic/Cars.elm + LaneSwitch.Elm
-- SVG drawing: Drawing/*.elm


-- called upon start
init : (Model, Cmd a)
init =
  ( initialModel |> initialSetup , Cmd.none) -- see Initial.elm


-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> init

    Pause -> ( {model | pause = not model.pause }, Cmd.none)

    -- update model, generation random numbers
    TimerNext time ->
      if not model.pause then
         (updateModel model, Cmd.batch (Interaction.RandomEffects.randomNumbers model) )
      else
        (model, Cmd.none)

    -- user interaction
    SwitchLight laneNo lightsNo ->
      ( { model |
            lanes = Interaction.User.switchLights model.lanes laneNo lightsNo }, Cmd.none)

    -- update car spawn probability
    CarProbability randomList ->
      let
        randomArray = Array.fromList randomList
        laneZippedWithRandom = Array.Extra.map2 (,) model.lanes randomArray -- zip lanes + randoms
      in
        ({ model |
             lanes = laneZippedWithRandom |> Array.map Interaction.RandomEffects.addNewCar  }, Cmd.none)

    -- update junction probability
    TurnProbability laneId randomFloats ->
      ({model | lanes = Interaction.RandomEffects.updateTurns laneId model.lanes (Array.fromList randomFloats)}, Cmd.none)


updateModel: Model -> Model
updateModel model =
  { model |
      lanes = model.lanes |> Array.map Logic.Cars.updateCars |> Logic.LaneSwitch.processCarLaneSwitch |> Array.map Logic.Cars.processCarMove,
      gameOver = model.lanes |> Logic.Game.checkBackLog backlogLimit lanesOverLimit,
      pause = model.pause || model.gameOver
  }


-- autoplay timer : use 15 milliseconds for 60 fps
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (15 * Time.millisecond) TimerNext


-- svg view
view : Model -> Html Msg
view model =

        div [bodyStyle]
        [ h2 [floatLeftStyle] [Html.text "Some Traffic on Elm Street : "]
          , gameStatus model.gameOver model.pause
          , button [ onClick Reset, floatRightStyle] [ Html.text "Reset" ]
            , br [] []
            , br [] []
          , checkbox Pause "Pause"

          , br [clearStyle] []

          , div [svgBoxStyle] [
              div [lawnStyle] [],
              div [svgStyle]
              [
                svg [ viewBox ("0 0 "++(Basics.toString cityMapWidth)++" "++ (Basics.toString cityMapHeight)),
                               Svg.Attributes.width ((toString (cityMapWidth))++"px"),
                               Svg.Attributes.height ((toString cityMapHeight)++"px")]
                (
                model.svgLanes -- streets
                ++ (Array.toList (Array.indexedMap Drawing.Lights.drawLightElements model.lanes) |> List.foldr (++) []) -- lights
                ++ (model.lanes |> Array.filter (\lane -> lane.spawn) |> Array.map Drawing.Lanes.drawLaneBacklog |> Array.foldr (++) []) --
                ++
                (  model.lanes |> Array.map (\lane -> List.map (Drawing.Cars.svgCarBox lane) lane.cars  |> List.foldr (++) [] ) |> Array.foldr (++) [] )-- flatmap
                )
              ]
          ]

        ]

gameStatus: Bool -> Bool -> Html Msg
gameStatus gameOver paused =
  if gameOver then
    h2 [floatLeftStyle, redColorStyle] [Html.text "GAME OVER"]
  else
    if paused then
      h2 [floatLeftStyle] [Html.text "paused"]
    else
      h2 [floatLeftStyle] [Html.text "Switch lights!"]


-- html checkbox
checkbox : Msg -> String -> Html Msg
checkbox msg title =
    label [floatRightStyle] [ input [ Html.Attributes.type_ "checkbox", onClick msg ] [], Html.text title ]


bodyStyle =
   Html.Attributes.style [("backgroundColor", "black"), ("color","white"), ("width","100%"),("height","100%")]

lawnStyle =
   Html.Attributes.style [("position","absolute"),("backgroundColor", "green"),("width", "200px"), ("height","400px")]

svgBoxStyle =
  Html.Attributes.style [("margin","auto"),("width",(toString cityMapWidth)++"px"),("height",(toString cityMapHeight)++"px"),("position","relative"),("top","20px"),("backgroundColor", "orange"),("border","1px"),("border-color","white")]

svgStyle =
  Html.Attributes.style [("position","absolute")]

redColorStyle =
  Html.Attributes.style [("color","red")]

floatLeftStyle =
    Html.Attributes.style [("float","left"),("margin-right","1em")]

floatRightStyle =
  Html.Attributes.style [("float","right")]

clearStyle =
  Html.Attributes.style [("clear","both")]

-- main program
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
