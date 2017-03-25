module Main exposing (..)

import Html exposing (Html, button, div, span, text)
import Wheel exposing (wheelView)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }

type Model
    = AttackingPlayerEnter
    | DefendingPlayerEnter
    | CombatResolution

model : Model
model = AttackingPlayerEnter

type Msg = NoOP

update : Msg -> Model -> Model
update msg model = model

type alias Player =
    { power : Power
    , powerCards : List PowerCard
    }


type Power
    = Power0
    | Power1
    | Power2
    | Power3
    | Power4
    | Power5
    | Power6
    | Power7
    | Power8
    | Power9
    | Power10
    | Power11
    | Power12
    | Power13
    | Power14
    | Power15
    | Power16


type PowerCard
    = PowerCard2
    | PowerCard3
    | PowerCard4
    | PowerCard5


view : Model -> Html Msg
view model = wheelView model
