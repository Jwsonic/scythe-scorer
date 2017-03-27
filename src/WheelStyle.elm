module WheelStyle exposing (wheelExtStyle, wheelStyle, wheelDivStyle)

import Css exposing (..)
import Html
import Html.Attributes


style : List Css.Mixin -> Html.Attribute msg
style =
    Css.asPairs >> Html.Attributes.style


wheelExtStyle : Html.Attribute msg
wheelExtStyle =
    style [ boxShadow5 (px 8) (px 6) (px 15) (px -6) (rgb 34 34 34) ]


wheelStyle : Html.Attribute msg
wheelStyle =
    style
        [ position absolute
        , height (px 500)
        , width (px 500)
        , borderRadius (pct 50)
        , backgroundColor (rgb 204 204 204)
        , overflow hidden
        ]


wheelDivStyle : Html.Attribute msg
wheelDivStyle =
    style
        [ position absolute
        , left (px 100)
        , bottom (px -10)
        , borderStyle solid
        , boxSizing borderBox
        , borderTopWidth (px 260)
        , borderBottomWidth (px 260)
        , borderRightWidth (px 150)
        , borderLeftWidth (px 150)
        ]



-- #wheel div {
--   position: absolute;
--   left: 100px;
--   bottom: -10px;
--   -webkit-transform-origin: 50% 50%;
--   -moz-transform-origin: 50% 50%;
--   -ms-transform-origin: 50% 50%;
--   -o-transform-origin: 50% 50%;
--   transform-origin: 50% 50%;
--   border-style: solid;
--   border-width: 260px 150px;
--   box-sizing: border-box;
-- }
