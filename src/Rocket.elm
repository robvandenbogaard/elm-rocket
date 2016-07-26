module Rocket exposing (..)

import Html.App as App
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard exposing (KeyCode)
import Key exposing (..)
import AnimationFrame
import Time exposing (Time)

type alias Movable = { x : Int, y : Int }
type alias Rocket = Movable

initialRocket : Rocket
initialRocket = { x = 0, y = 0 }

type alias Missile = Movable

initialMissile : Missile
initialMissile = { x = 0, y = 0 }

type alias Universe = { rocket: Rocket, missiles: List Missile }

initialUniverse : Universe
initialUniverse = { rocket = initialRocket, missiles = [] }

rocketSprite : Rocket -> Svg msg
rocketSprite rocket =
  svg [ Svg.Attributes.style ("position: fixed; bottom: 0; left: " ++ (toString rocket.x)), version "1.1", x "0", y "0", width "100px", viewBox "25 0 50 100" ]
  [
    g [ fill "#5596c9" ]
    [
      Svg.path [ d "m 40.541,90.855 c 0.362,1.285 0.729,2.51 1.091,3.674 h 16.734 c 0.362,-1.165 0.728,-2.389 1.091,-3.674 H 40.541 z" ] [],
      Svg.path [ d "M 65.094,67.932 C 68.209,51.667 69.211,33.352 61.487,17.035 H 38.511 c -7.722,16.316 -6.723,34.632 -3.606,50.897 -1.655,2.288 -9.073,10.48 -9.073,10.48 L 27.189,100 h 4.43 l 3.425,-14.012 3.729,-1.609 c 0.422,1.574 0.835,3.089 1.244,4.569 h 19.965 c 0.407,-1.48 0.823,-2.997 1.244,-4.569 L 64.955,85.988 68.38,100 h 4.431 l 1.357,-21.588 c 0,0 -7.421,-8.192 -9.074,-10.48 z M 50,29.86 c 2.665,0.052 4.808,2.178 4.808,4.807 0,2.626 -2.143,4.758 -4.808,4.809 -2.666,-0.051 -4.808,-2.183 -4.808,-4.809 0,-2.629 2.142,-4.755 4.808,-4.807 z" ] [],
      Svg.path [ d "m 49.974,15.125 h 0.054 10.483 C 57.853,10.134 50.23,0 49.975,0 49.718,0 42.149,10.134 39.492,15.125 h 10.482 z" ] []
    ]
  ]

missileSprite : Missile -> Svg msg
missileSprite missile =
  svg [ Svg.Attributes.style ("position: fixed; bottom: " ++ (toString missile.y) ++ "px; left: " ++ (toString missile.x)), version "1.1", x "0", y "0", width "100px", viewBox "25 0 50 100" ]
  [
    g [ fill "yellow" ]
    [
      Svg.path [ d "m 40.541,90.855 c 0.362,1.285 0.729,2.51 1.091,3.674 h 16.734 c 0.362,-1.165 0.728,-2.389 1.091,-3.674 H 40.541 z" ] []
    ]
  ]

missileSprites : List Missile -> Html msg
missileSprites missiles =
  Html.div [] (List.map missileSprite missiles)

type Msg =
  TimeUpdate Time |
  KeyDown KeyCode

moveRocket : Int -> Universe -> Universe
moveRocket amount universe =
  { universe | rocket = move amount universe.rocket }

move : Int -> { a | x : Int } -> { a | x : Int }
move amount movable =
  { movable | x = movable.x + amount }

fireMissile universe =
  { universe | missiles = { x = universe.rocket.x, y = 200 + universe.rocket.y } :: universe.missiles }

advanceMissile : Float -> Missile -> Missile
advanceMissile dt model =
  { model | y = model.y + 10 }

cleanupMissiles : List Missile -> List Missile
cleanupMissiles missiles =
  List.filter missileNeedsToGo missiles

missileNeedsToGo : Missile -> Bool
missileNeedsToGo missile =
  missile.y < 500

applyPhysics : Float -> Universe -> Universe
applyPhysics dt model =
  { model | missiles = (List.map (advanceMissile dt) model.missiles) |> cleanupMissiles }

applyEffectFromKey : KeyCode -> Universe -> Universe
applyEffectFromKey keyCode universe =
  case Key.fromCode keyCode of
    ArrowLeft -> moveRocket -10 universe
    ArrowRight -> moveRocket 10 universe
    Space -> fireMissile universe
    _ -> universe

init : (Universe, Cmd msg)
init =
  (initialUniverse, Cmd.none)

update : Msg -> Universe -> (Universe, Cmd msg)
update msg universe =
  case msg of
    TimeUpdate dt -> (applyPhysics dt universe, Cmd.none)
    KeyDown keyCode -> (applyEffectFromKey keyCode universe, Cmd.none)

subscriptions : Universe -> Sub Msg
subscriptions model =
  Sub.batch
  [
    Keyboard.downs KeyDown,
    AnimationFrame.diffs TimeUpdate
  ]

view : Universe -> Html msg
view universe =
  Html.div [ HtmlAttr.style [ ("background", "black"), ("width", "100%"), ("height", "100%") ] ] [
    rocketSprite universe.rocket,
    missileSprites universe.missiles
  ]

main =
  App.program { init = init, update = update, subscriptions = subscriptions, view = view }
