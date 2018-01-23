module Drawing.Lights exposing (drawLightElements)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Types exposing (..)
import Constants exposing (..)


-- called from html view
drawLightElements: Int -> Lane -> List (Svg Msg)
drawLightElements laneId lane =
  lane.lights |> Array.indexedMap (\lightNo light -> svgLight lightNo light laneId lane.startCoord lane.direction lane.distance ) |> Array.toList |> List.foldr (++) []

svgLight: Int-> Light -> Int -> Point -> Direction -> Int -> List (Svg Msg)
svgLight lightId light laneId laneStart dir distance =
  let
    lightPos =
      case dir of
        East -> { x=laneStart.x + light.p ,
                   y=laneStart.y + laneWidth }
        West -> { x=laneStart.x + distance - light.p ,
                    y=laneStart.y - laneWidth }
        South -> { x=laneStart.x - laneWidth ,
                   y=laneStart.y + light.p - lightStreetSpacing}
        North -> { x=laneStart.x + laneWidth,
                    y=laneStart.y + distance - light.p } -- + 2* lightStreetSpacing}

    lightRotation =
      case dir of
        East -> 90
        West -> -90
        South -> 180
        North -> 0

    rotationTransform = "rotate("++ (toString lightRotation) ++"," ++ (toString (lightPos.x)) ++ "," ++ (toString (lightPos.y)) ++")"

    lightSvgBox = rect [ x (toString lightPos.x),
              y (toString lightPos.y),
              Svg.Attributes.width (toString lightWidth),
              Svg.Attributes.height (toString lightHeight),
              transform (rotationTransform),
              onClick (SwitchLight laneId lightId),
              -- case light.left of
              --   Nothing -> fill "white"
              --   _ -> fill "#303030"
              fill "#303030"
                 ] []

    lightRedSvg = circle [ cx (toString (lightPos.x+lightFireCenterBothX)),
                           cy (toString (lightPos.y+lightFireCenterBothX)),
                           r (toString lightFireRadius),
                           onClick (SwitchLight laneId lightId),
                           transform (rotationTransform),
                           fill (lightFireFill (Red light.on))  ] []
    lightGreenSvg = circle [ cx (toString (lightPos.x+lightFireCenterBothX)),
                             cy (toString (lightPos.y+lightFireCenterGreenY)),
                             r (toString lightFireRadius),
                             onClick (SwitchLight laneId lightId),
                             transform (rotationTransform),
                             fill (lightFireFill (Green (not light.on))) ] []

    -- lightTextSvg = Svg.text_ [x (toString lightPos.x), y (toString lightPos.y), color "white"] [Svg.text (debugLight light laneId)]
  in
    [lightSvgBox, lightRedSvg, lightGreenSvg] --, lightTextSvg]

debugLight: Light -> Int -> String
debugLight light laneId =
  let
    leftStr =
      case light.left of
        Nothing -> ""
        Just id -> " l:"++ (toString id)
    rightStr =
      case light.right of
        Nothing -> ""
        Just id -> " r:"++ (toString id)
    straightStr =
      case light.straight of
        False -> ""
        True -> " s: " ++ (toString laneId)
   in
     leftStr ++ rightStr ++ straightStr

lightFireFill: LightFire -> String
lightFireFill lightfire =
  case lightfire of
    Red False -> "#770000"
    Red True -> "#ff0000"
    Green False -> "#007700"
    Green True -> "#00ff00"
