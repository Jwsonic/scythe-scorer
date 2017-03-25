module WheelStyle exposing (..)

import Css exposing (..)
import Html
import Html.Attributes

style : List Css.Mixin -> Html.Attribute msg
style =
    Css.asPairs >> Html.Attributes.style

wheelExtStyle : Html.Attribute msg
wheelExtStyle = style [boxShadow5 (px 8) (px 6) (px 15) (px -6) (rgb 34 34 34)]
