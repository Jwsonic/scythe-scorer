module Main exposing (..)

import Html exposing (Html, button, div, span, text, img)
import Wheel exposing (wheelView)
import Msg exposing (..)
import Model exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


model : Model
model =
    AttackingPlayerEnter 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        WheelClick { clientX, clientY } ->
            AttackingPlayerEnter <| (atan2 (clientY - 180) (clientX - 180)) * 180 / pi + 90

        _ ->
            model


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
view model =
    wheelView model
