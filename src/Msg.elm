module Msg exposing (..)


type alias MouseClickPoint =
    { clientX : Float, clientY : Float }


type Msg
    = WheelClick MouseClickPoint
    | NoOp
