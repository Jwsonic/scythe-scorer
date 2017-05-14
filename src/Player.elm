module Player
    exposing
        ( Player
        , initPlayer
        , totalPower
        , getPower
        , getCards
        )

import Html exposing (..)
import Html.SelectPrism exposing (selectp)
import Power exposing (..)
import Card exposing (..)
import List.Extra exposing (removeAt)


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
            player


{-| setPower sets the player's new power if it's a valid power
-}
setPower : Result e Power -> Player -> Player
setPower result ((Player oldPower cards) as player) =
    case result of
        Ok newPower ->
            if isValidPower newPower then
                Player newPower cards
            else
                player

        Err _ ->
            player


{-| addCard will add a card to a player's used cards if it's a valid card
-}
addCard : Result e Card -> Player -> Player
addCard result ((Player power cards) as player) =
    case result of
        Ok newCard ->
            if isValidCard newCard then
                Player power (newCard :: cards)
            else
                player

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


{-| options for the power <select />
-}
powerOptions : List ( String, Power )
powerOptions =
    List.map (\p -> ( powerToLabel p, p )) allPowers


powerSelect : Player -> Html Msg
powerSelect (Player power cards) =
    selectp powerPrism SetPower power [] powerOptions


cardOptions : List ( String, Card )
cardOptions =
    List.map (\c -> ( cardToLabel c, c )) allCards


cardSelect : Html Msg
cardSelect =
    selectp cardPrism AddCard 0 [] cardOptions
