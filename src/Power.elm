module Power exposing (..)

import Monocle.Prism exposing (Prism)
import Point exposing (..)


type alias Power =
    Point


{-| all possible values for the power type
-}
allPowers : List Power
allPowers =
    List.range 0 7


{-| internal use only error message constructor for power
-}
errMsg : Power -> String
errMsg power =
    (toString power) ++ " is not a valid power"


{-| intToPower attempts to convert an Int into a Power
-}
intToPower : Int -> Result String Power
intToPower =
    intToPoint errMsg allPowers


{-| stringToPower attemps to convert a String into a Point
-}
stringToPower : String -> Result String Power
stringToPower =
    stringToPoint intToPower


{-| pointPrism is a Prism for converting between a String and a Point
-}
powerPrism : Prism String Power
powerPrism =
    pointPrism stringToPower


{-| powerToLabel generates a label String for a Power
-}
powerToLabel : Power -> String
powerToLabel power =
    "Power " ++ (toString power)
