module PowerTests exposing (..)

import Expect
import Power exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Power"
        [ getOptionTests ]


reverseGetTests : Test
reverseGetTests =
    describe "powerPrism.reverseGet" <|
        List.map testReverseGet allPowers


testReverseGet : Power -> Test
testReverseGet power =
    let
        strPower =
            toString power
    in
        test ("power " ++ strPower) <|
            \_ ->
                powerPrism.reverseGet power
                    |> Expect.equal strPower


getOptionTests : Test
getOptionTests =
    describe "powerPrism.getOption" <|
        List.append
            (List.map
                (testGetOption True)
                allPowers
            )
            (List.map
                (testGetOption False)
                [ -5, -1, 8, 10 ]
            )


testGetOption : Bool -> Power -> Test
testGetOption pass power =
    let
        strPower =
            toString power

        expectation =
            if pass then
                Just power
            else
                Nothing
    in
        test ("power " ++ strPower) <|
            \_ ->
                powerPrism.getOption strPower
                    |> Expect.equal expectation
