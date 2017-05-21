module Point exposing (..)

import Monocle.Prism exposing (Prism)


type alias Point =
    Int


{-| Helper function that builds a function that attempts to transform an Int
into a Point Value. All valid Point values are defined as members of a List.
-}
intToPoint : (Int -> String) -> List Point -> Int -> Result String Point
intToPoint unitToErr validPoints unit =
    if List.member unit validPoints then
        Ok unit
    else
        Err <| unitToErr unit


{-| Helper function that uses a function build by intToPoint in order to
attempt to transform a String into a Point.
-}
stringToPoint : (Int -> Result String Point) -> String -> Result String Point
stringToPoint intToPoint string =
    string
        |> String.toInt
        |> Result.andThen intToPoint


{-| A prism for converting between String and Point
-}
pointPrism : (String -> Result String Point) -> Prism String Point
pointPrism stringToPoint =
    Prism (stringToPoint >> Result.toMaybe) toString
