module Scythe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, selected, value)
import Html.Events exposing (..)


{-| The valid power choices in Scythe are 0-7
-}
powers : List Int
powers =
    List.range 0 7


{-| isValidPower checks if an Int is a valid power value
-}
isValidPower : Int -> Bool
isValidPower =
    flip List.member powers


{-| In Scythe there are only 2,3,4 value card. The value 0 is used to represent no card selected
-}
cards : List Int
cards =
    [ 0, 2, 3, 4, 5 ]


{-| isValidCard checks if an Int is a valid card value
-}
isValidCard : Int -> Bool
isValidCard =
    flip List.member cards


type alias Power =
    Int


type alias Card =
    Int


type alias Player =
    { power : Power
    , card : Card
    }


type Page
    = Attacking
    | Defending
    | Results


type Msg
    = SetPower Power
    | SetCard Card
    | NextPage
    | Reset


type alias Model =
    { attackingPlayer : Player
    , defendingPlayer : Player
    , page : Page
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


initPlayer : Player
initPlayer =
    { power = 0, card = 0 }


initialModel : Model
initialModel =
    { attackingPlayer = initPlayer
    , defendingPlayer = initPlayer
    , page = Attacking
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Reset is the same for any page, just reset the model
        Reset ->
            initialModel

        NextPage ->
            { model | page = updatePage model.page }

        _ ->
            case model.page of
                Attacking ->
                    { model | attackingPlayer = updatePlayer msg model.attackingPlayer }

                Defending ->
                    { model | defendingPlayer = updatePlayer msg model.defendingPlayer }

                Results ->
                    model


updatePage : Page -> Page
updatePage page =
    case page of
        -- Defending always follows Attacking
        Attacking ->
            Defending

        -- Results always follows Defending(and Results)
        _ ->
            Results


{-| updatePlayer handles the data update for a specific player
-}
updatePlayer : Msg -> Player -> Player
updatePlayer msg player =
    case msg of
        SetPower newPower ->
            setPower player newPower

        SetCard newCard ->
            setCard player newCard

        _ ->
            player


{-| setPower sets the player's new power if it's a valid power
-}
setPower : Player -> Power -> Player
setPower player newPower =
    { player
        | power =
            if isValidPower newPower then
                newPower
            else
                player.power
    }


{-| setCard sets the player's new card if it's a calid card
-}
setCard : Player -> Card -> Player
setCard player newCard =
    { player
        | card =
            if isValidCard newCard then
                newCard
            else
                player.card
    }


view : Model -> Html Msg
view model =
    let
        body =
            case model.page of
                Attacking ->
                    attackingPlayerView model.attackingPlayer

                Defending ->
                    defendingPlayerView model.defendingPlayer

                Results ->
                    resultsView model
    in
        div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col" ]
                    body
                ]
            ]


{-| handles the drawing of the Results Page
-}
resultsView : Model -> List (Html Msg)
resultsView { attackingPlayer, defendingPlayer } =
    [ text <| (winnerText attackingPlayer defendingPlayer) ++ " player wins!"
    , br [] []
    , resetButton
    ]


{-| winnerText determines who the winner was and returns the proper text. Ties go to the attackingPlayer.
-}
winnerText : Player -> Player -> String
winnerText attackingPlayer defendingPlayer =
    let
        attackingTotal =
            attackingPlayer.power + attackingPlayer.card

        defendingtotal =
            defendingPlayer.power + defendingPlayer.card
    in
        if attackingTotal >= defendingtotal then
            "Attacking"
        else
            "Defending"


attackingPlayerView : Player -> List (Html Msg)
attackingPlayerView =
    playerView "Attacking players turn." "Next Player's Turn"


defendingPlayerView : Player -> List (Html Msg)
defendingPlayerView =
    playerView "Defending players turn." "View results"


{-| playerView handles drawing the player agnostic view for the current player
-}
playerView : String -> String -> Player -> List (Html Msg)
playerView titleText nextText player =
    let
        currentPowerStr =
            toString <| player.power + player.card
    in
        [ h1 [] [ text titleText ]
        , h2 [] [ text <| "Your current power is " ++ currentPowerStr ++ "." ]
        , selectPower player.power
        , selectCard player.card
        , resetButton
        , nextButton nextText
        ]


selectPower : Power -> Html Msg
selectPower power =
    selectForm (optionBuilder power powers) "power" (inputMapper SetPower)


selectCard : Card -> Html Msg
selectCard card =
    selectForm (optionBuilder card cards) "card" (inputMapper SetCard)


selectForm : List (Html msg) -> String -> (String -> msg) -> Html msg
selectForm options label_ mapper =
    div [ class "form-group" ]
        [ label [ for "power" ] [ text <| "Select your " ++ label_ ]
        , select
            [ onInput mapper
            , id label_
            , class "form-control"
            ]
            options
        ]


{-| inputMapper turns a <select> change into a valid Msg
-}
inputMapper : (Int -> Msg) -> String -> Msg
inputMapper msg str =
    String.toInt str |> Result.withDefault 0 |> msg


optionBuilder : Int -> List Int -> List (Html Msg)
optionBuilder i =
    let
        makeOption n =
            option
                [ selected <| n == i
                , value <| toString n
                ]
                [ text <| toString n ]
    in
        List.map makeOption


resetButton : Html Msg
resetButton =
    button
        [ onClick Reset
        , class "btn btn-danger"
        ]
        [ text "Reset" ]


nextButton : String -> Html Msg
nextButton string =
    button
        [ onClick NextPage
        , class "btn btn-primary"
        ]
        [ text string ]
