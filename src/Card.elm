module Card exposing (..)

import Monocle.Prism exposing (Prism)


type alias Card =
    Int


allCards : List Card
allCards =
    List.range 2 5


isValidCard : Card -> Bool
isValidCard card =
    List.member card allCards


intToCard : Int -> Result String Card
intToCard int =
    if isValidCard int then
        Ok int
    else
        Err <| (toString int) ++ " is not a valid card"


stringToCard : String -> Result String Card
stringToCard string =
    string
        |> String.toInt
        |> Result.andThen intToCard


cardPrism : Prism String Card
cardPrism =
    let
        maybeCard : String -> Maybe Card
        maybeCard =
            stringToCard >> Result.toMaybe
    in
        Prism maybeCard toString


cardToLabel : Card -> String
cardToLabel card =
    "Card " ++ (toString card)
