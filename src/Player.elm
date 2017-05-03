module Player
    exposing
        ( Player
        , Card
        , Power
        , allPower
        , allCards
        , initPlayer
        , setPower
        , addCard
        , removeCard
        , totalPower
        , getPower
        , getCards
        , stringToCard
        , stringToPower
        )


type alias Card =
    Int


type alias Power =
    Int


allPower : List Power
allPower =
    List.range 0 7


{-| String to power is a helper that tries to convert a string to a power.
  If the string is not a valid power, it will will return a known invalid
  power that will cause a NoOP of all player methods.
-}
stringToPower : String -> Power
stringToPower string =
    String.toInt string |> Result.withDefault -1


isValidPower : Power -> Bool
isValidPower =
    flip List.member allPower


allCards : List Card
allCards =
    [ 2, 3, 4, 5 ]


invalidCard : Card
invalidCard =
    -1


{-| String to card is a helper that tries to convert a string to a card.
  If the string is not a valid card, it will will return a known invalid
  card that will cause a NoOP of all player methods.
-}
stringToCard : String -> Card
stringToCard string =
    String.toInt string |> Result.withDefault -1


isValidCard : Card -> Bool
isValidCard =
    flip List.member allCards


{-| Type representation for a player. Power is and aliased Int, while the player's cards are stored as a list of aliased Ints
-}
type Player
    = Player Power (List Card)


initPlayer : Player
initPlayer =
    Player 0 []


{-| setPower sets the player's new power if it's a valid power
-}
setPower : Power -> Player -> Player
setPower newPower ((Player oldPower cards) as player) =
    if isValidPower newPower then
        Player newPower cards
    else
        player


{-| add a card to a player's used cards
-}
addCard : Card -> Player -> Player
addCard newCard ((Player power cards) as player) =
    if isValidCard newCard then
        Player power (newCard :: cards)
    else
        player


{-| removeCard removes the card at the given index
-}
removeCard : Int -> Player -> Player
removeCard index (Player power cards) =
    Player power (removeAt index cards)


{-| removeAt from the elm-community/list-extra package
-}
removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l
    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
            case tail of
                Nothing ->
                    l

                Just t ->
                    List.append head t


{-| returns the total power for a player. ie power dial + cards
-}
totalPower : Player -> Int
totalPower (Player power cards) =
    power + List.sum cards


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
