module Wheel exposing (..)

import Css exposing (deg, rotate, transform)
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on)
import Json.Decode as Json
import Msg exposing (..)
import Model exposing (..)


style : List Css.Mixin -> Html.Attribute Msg
style =
    Css.asPairs >> Html.Attributes.style


wheelView : Model -> Html Msg
wheelView model =
    let
        rotation =
            case model of
                AttackingPlayerEnter r ->
                    r

                _ ->
                    0
    in
        img
            [ src "power_dial_1.png"
            , style [ transform <| rotate <| deg rotation ]
            , onWheelClick
            ]
            []


onWheelClick : Attribute Msg
onWheelClick =
    on "click" (Json.map WheelClick mouseClickPoint)


mouseClickPoint : Json.Decoder MouseClickPoint
mouseClickPoint =
    Json.map2 MouseClickPoint
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
