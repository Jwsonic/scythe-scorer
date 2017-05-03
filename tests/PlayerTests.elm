module PlayerTests exposing (..)

import Expect
import Player exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Player"
        [ totalPowerTests, setPowerTests, addCardTests, removeCardTests ]


totalPowerTests : Test
totalPowerTests =
    describe "totalPower"
        [ test "player with power and no cards" <|
            \_ ->
                initPlayer
                    |> setPower 5
                    |> totalPower
                    |> Expect.equal 5
        , test "player with cards and no power" <|
            \_ ->
                initPlayer
                    |> addCard 3
                    |> addCard 2
                    |> totalPower
                    |> Expect.equal 5
        , test "player with cards and power" <|
            \_ ->
                initPlayer
                    |> setPower 4
                    |> addCard 3
                    |> addCard 3
                    |> addCard 2
                    |> totalPower
                    |> Expect.equal 12
        ]


setPowerTests : Test
setPowerTests =
    describe "setPower"
        [ test "setting an invalid power doesn't work" <|
            \_ -> setPower -1 initPlayer |> Expect.equal initPlayer
        , test "setting a valid power works" <|
            \_ ->
                setPower 5 initPlayer
                    |> getPower
                    |> Expect.equal 5
        ]


addCardTests : Test
addCardTests =
    describe "addCard"
        [ test "adding a valid card works" <|
            \_ ->
                addCard 4 initPlayer
                    |> addCard 2
                    |> getCards
                    |> Expect.equal [ 2, 4 ]
        , test "adding an invalid cards does not work" <|
            \_ ->
                addCard -4 initPlayer
                    |> getCards
                    |> Expect.equal []
        ]


removeCardTests : Test
removeCardTests =
    describe "removeCard"
        [ test "removing a card in the player's hand works" <|
            \_ ->
                addCard 4 initPlayer
                    |> addCard 2
                    |> addCard 2
                    |> removeCard 1
                    |> getCards
                    |> Expect.equal [ 2, 4 ]
        , test "removing a card not in the player's hand does nothing" <|
            \_ ->
                addCard 4 initPlayer
                    |> addCard 2
                    |> addCard 2
                    |> removeCard 10
                    |> getCards
                    |> Expect.equal [ 2, 2, 4 ]
        ]
