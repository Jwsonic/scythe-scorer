module Scythe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (..)


type alias Player =
    { power : Int
    , card : Int
    }


type Page
    = Attacking
    | Defending
    | Results


type Msg
    = SetPower Int
    | SetCard Int
    | SetPage Page
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
        Reset ->
            initialModel

        SetPage newPage ->
            { model | page = newPage }

        _ ->
            case model.page of
                Attacking ->
                    { model | attackingPlayer = updatePlayer msg model.attackingPlayer }

                Defending ->
                    { model | defendingPlayer = updatePlayer msg model.defendingPlayer }

                Results ->
                    model


updatePlayer : Msg -> Player -> Player
updatePlayer msg player =
    case msg of
        SetPower newPower ->
            setPlayerPower player newPower

        SetCard newCard ->
            setPlayerCard player newCard

        _ ->
            player


setPlayerPower : Player -> Int -> Player
setPlayerPower player newPower =
    { player | power = newPower |> max 0 |> min 7 }


setPlayerCard : Player -> Int -> Player
setPlayerCard player newCard =
    let
        checkedNewCard =
            if newCard < 2 then
                0
            else
                newCard |> max 2 |> min 5
    in
        { player | card = checkedNewCard }


view : Model -> Html Msg
view model =
    case model.page of
        Attacking ->
            playerView model.attackingPlayer

        Defending ->
            playerView model.defendingPlayer

        Results ->
            resultsView model


resultsView : Model -> Html Msg
resultsView { attackingPlayer, defendingPlayer } =
    let
        attackingTotal =
            attackingPlayer.power + attackingPlayer.card

        defendingtotal =
            defendingPlayer.power + defendingPlayer.card

        winnerText =
            if attackingTotal >= defendingtotal then
                "Attacking"
            else
                "Defending"
    in
        div []
            [ text <| winnerText ++ "won!"
            , resetButton
            ]


playerView : Player -> Html Msg
playerView player =
    let
        currentPowerStr =
            toString <| player.power + player.card
    in
        div []
            [ text <| "Your current power is " ++ currentPowerStr
            , powerDial player.power
            , powerCard player.card
            , resetButton
            ]


powerDial : Int -> Html Msg
powerDial power =
    let
        options =
            List.range 0 7 |> optionBuilder power

        inputMapper str =
            case String.toInt str of
                Ok power ->
                    SetPower power

                Err _ ->
                    SetPower 0
    in
        select
            [ onInput inputMapper
            ]
            options


powerCard : Int -> Html Msg
powerCard card =
    let
        options =
            optionBuilder card [ 0, 2, 3, 4, 5 ]

        inputMapper str =
            case String.toInt str of
                Ok card ->
                    SetCard card

                Err _ ->
                    SetPower 0
    in
        select
            [ onInput inputMapper
            ]
            options


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
    button [ onClick Reset ] [ text "Reset" ]
