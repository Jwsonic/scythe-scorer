module Scythe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (..)


powers : List Int
powers =
    List.range 0 7


isValidPower : Int -> Bool
isValidPower =
    flip List.member powers


cards : List Int
cards =
    [ 0, 2, 3, 4, 5 ]


isValidCard : Int -> Bool
isValidCard =
    flip List.member cards


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
        Attacking ->
            Defending

        _ ->
            Results


updatePlayer : Msg -> Player -> Player
updatePlayer msg player =
    case msg of
        SetPower newPower ->
            setPower player newPower

        SetCard newCard ->
            setCard player newCard

        _ ->
            player


setPower : Player -> Int -> Player
setPower player newPower =
    { player
        | power =
            if isValidPower newPower then
                newPower
            else
                0
    }


setCard : Player -> Int -> Player
setCard player newCard =
    { player
        | card =
            if isValidCard newCard then
                newCard
            else
                0
    }


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
            optionBuilder power powers
    in
        select
            [ onInput <| inputMapper SetPower
            ]
            options


powerCard : Int -> Html Msg
powerCard card =
    let
        options =
            optionBuilder card cards
    in
        select
            [ onInput <| inputMapper SetCard
            ]
            options


inputMapper : (Int -> Msg) -> String -> Msg
inputMapper msg str =
    case String.toInt str of
        Ok card ->
            msg card

        Err _ ->
            msg 0


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
