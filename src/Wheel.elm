module Wheel exposing (wheelView)

import WheelStyle exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, id)

wheelView :  model -> Html msg
wheelView model = div [ id "wheelExt", wheelExtStyle ]
    [ div [ id "wheel" ]
        [ div [ class "wheel_part color01" ]
            []
        , div [ class "wheel_part color02" ]
            []
        , div [ class "wheel_part color03" ]
            []
        , div [ class "wheel_part color04" ]
            []
        , div [ class "wheel_part word color05" ]
            []
        , div [ class "wheel_part word color06" ]
            []
        , span [ class "wheel_part word word01" ]
            [ text "Spinning" ]
        , span [ class "wheel_part word word02" ]
            [ text "The" ]
        , span [ class "wheel_part word word03" ]
            [ text "Circle" ]
        , span [ class "wheel_part word word04" ]
            [ text "Is" ]
        , span [ class "wheel_part word word05" ]
            [ text "Super" ]
        , span [ class "wheel_part word word06" ]
            [ text "Exciting" ]
        ]
    ]
