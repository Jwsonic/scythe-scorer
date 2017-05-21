module Card exposing (..)

import Monocle.Prism exposing (Prism)
import Point exposing (..)


type alias Card =
    Point


{-| allCards is a List of every valid Card value
-}
allCards : List Card
allCards =
    List.range 2 5


{-| errMsg is an internal function used to construct a Card error message
-}
errMsg : Card -> String
errMsg card =
    (toString card) ++ " is not a valid card"


{-| intToCard attempts to transform and Int into a Card
-}
intToCard : Int -> Result String Card
intToCard =
    intToPoint errMsg allCards


{-| stringToCard attempts to transform a String into a Card
-}
stringToCard : String -> Result String Card
stringToCard =
    stringToPoint intToCard


{-| cardPrism is a Prism for converting between a String and a Card
-}
cardPrism : Prism String Card
cardPrism =
    pointPrism stringToCard


{-| cardToLabel is a function for generating a label String froma  given power
-}
cardToLabel : Card -> String
cardToLabel card =
    "Card " ++ (toString card)
