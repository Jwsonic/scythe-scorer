module Tests exposing (..)

import Test exposing (..)
import Expect


-- import Fuzz exposing (list, int, tuple, string)
-- import String

import Scythe exposing (..)


all : Test
all =
    describe "Sycthe Scorer"
        [ describe "setPlayerPower"
            [ test "Can't add power < 0" <|
                \() ->
                    Expect.equal (setPlayerPower initPlayer -1) initPlayer
            , test "Cant' add power > 7" <|
                \() ->
                    Expect.equal (setPlayerPower initPlayer 8) { power = 7, card = 0 }
            ]
        , describe "setPlayerCard"
            [ test "Can't add card < 0" <|
                \() ->
                    Expect.equal (setPlayerCard initPlayer -1) initPlayer
            , test "Cant' add card > 5" <|
                \() ->
                    Expect.equal (setPlayerCard initPlayer 6) { power = 0, card = 5 }
            , test "Can't add card == 1" <|
                \() ->
                    Expect.equal (setPlayerCard initPlayer 1) initPlayer
            ]
        ]
