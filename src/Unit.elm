module Unit exposing (..)

import Monocle.Prism exposing (Prism)


intToUnit : (Int -> String) -> List Int -> Int -> Result String Int
intToUnit unitToErr validUnits unit =
    if List.member unit validUnits then
        Ok unit
    else
        Err <| unitToErr unit


stringToUnit : (Int -> Result String Int) -> String -> Result String Int
stringToUnit intToUnit string =
    string
        |> String.toInt
        |> Result.andThen intToUnit


unitPrism : (String -> Result String Int) -> Prism String Int
unitPrism stringToUnit =
    Prism (stringToUnit >> Result.toMaybe) toString
