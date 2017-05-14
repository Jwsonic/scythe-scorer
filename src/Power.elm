module Power exposing (..)

import Monocle.Prism exposing (Prism)
import Unit exposing (..)


type alias Power =
    Int


allPowers : List Power
allPowers =
    List.range 0 7


errMsg : Power -> String
errMsg power =
    (toString power) ++ " is not a valid power"


intToPower : Int -> Result String Power
intToPower =
    intToUnit errMsg allPowers


stringToPower : String -> Result String Power
stringToPower =
    stringToUnit intToPower


powerPrism : Prism String Power
powerPrism =
    unitPrism stringToPower


powerToLabel : Power -> String
powerToLabel power =
    "Power " ++ (toString power)
