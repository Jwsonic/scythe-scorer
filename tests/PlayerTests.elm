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
                    |> updatePlayer (SetPower <| Ok 3)
                    |> totalPower
                    |> Expect.equal 3
        , test "player with cards and no power" <|
            \_ ->
                initPlayer
                    |> updatePlayer (AddCard <| Ok 3)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> totalPower
                    |> Expect.equal 5
        , test "player with cards and power" <|
            \_ ->
                initPlayer
                    |> updatePlayer (SetPower <| Ok 4)
                    |> updatePlayer (AddCard <| Ok 3)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> totalPower
                    |> Expect.equal 9
        , test "player with no cards and no power" <|
            \_ ->
                initPlayer
                    |> totalPower
                    |> Expect.equal 0
        ]


setPowerTests : Test
setPowerTests =
    describe "SetPower"
        [ test "setting an invalid power doesn't work" <|
            \_ ->
                initPlayer
                    |> updatePlayer (SetPower <| Ok -1)
                    |> Expect.equal initPlayer
        , test "setting a valid power works" <|
            \_ ->
                initPlayer
                    |> updatePlayer (SetPower <| Ok 5)
                    |> getPower
                    |> Expect.equal 5
        ]


addCardTests : Test
addCardTests =
    describe "addCard"
        [ test "adding a valid card works" <|
            \_ ->
                initPlayer
                    |> updatePlayer (AddCard <| Ok 5)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> getCards
                    |> Expect.equal [ 2, 5 ]
        , test "adding an invalid cards does not work" <|
            \_ ->
                initPlayer
                    |> updatePlayer (AddCard <| Ok -1)
                    |> getCards
                    |> Expect.equal []
        ]


removeCardTests : Test
removeCardTests =
    describe "removeCard"
        [ test "removing a card in the player's hand works" <|
            \_ ->
                initPlayer
                    |> updatePlayer (AddCard <| Ok 4)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> updatePlayer (RemoveCard 1)
                    |> getCards
                    |> Expect.equal [ 2, 4 ]
        , test "removing a card not in the player's hand does nothing" <|
            \_ ->
                initPlayer
                    |> updatePlayer (AddCard <| Ok 4)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> updatePlayer (AddCard <| Ok 2)
                    |> updatePlayer (RemoveCard 10)
                    |> getCards
                    |> Expect.equal [ 2, 2, 4 ]
        ]
