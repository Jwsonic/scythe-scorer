module Player
    exposing
        ( Player
        , initPlayer
        , totalPower
        , updatePlayer
        , Msg(..)
        , getPower
        , getCards
        )

import Card exposing (..)
import List.Extra exposing (removeAt)
import Power exposing (..)
import Result exposing (andThen)


type Msg
    = SetPower (Result String Power)
    | AddCard (Result String Card)
    | RemoveCard Int


{-| Type representation for a player. Power is and aliased Int, while the player's cards are stored as a list of aliased Ints
-}
type Player
    = Player Power (List Card)


initPlayer : Player
initPlayer =
    Player 0 []


updatePlayer : Msg -> Player -> Player
updatePlayer msg player =
    case msg of
        SetPower result ->
            setPower result player

        AddCard result ->
            addCard result player

        RemoveCard index ->
            removeCard index player


{-| setPower sets the player's new power if it's a valid power
-}
setPower : Result String Power -> Player -> Player
setPower resultPower ((Player oldPower cards) as player) =
    let
        resultPower_ =
            resultPower |> andThen intToPower
    in
        case resultPower_ of
            Ok newPower ->
                Player newPower cards

            Err _ ->
                player


{-| addCard will add a card to a player's used cards if it's a valid card
-}
addCard : Result String Card -> Player -> Player
addCard resultCard ((Player power cards) as player) =
    let
        resultCard_ =
            resultCard |> andThen intToCard
    in
        case resultCard_ of
            Ok newCard ->
                Player power (cards ++ [ newCard ])

            Err _ ->
                player


{-| removeCard removes the card at the given index
-}
removeCard : Int -> Player -> Player
removeCard index (Player power cards) =
    Player power (removeAt index cards)


{-| returns the total power for a player. ie power dial + cards
-}
totalPower : Player -> Int
totalPower (Player power cards) =
    power + (List.sum cards)


{-| returns the cards in the players hand
-}
getCards : Player -> List Card
getCards (Player power cards) =
    cards


{-| returns the power for the player
-}
getPower : Player -> Power
getPower (Player power cards) =
    power
