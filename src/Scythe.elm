module Scythe exposing (..)

import Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, for, id, selected, type_, value)
import Html.Events exposing (..)
import Html.SelectPrism exposing (selectp)
import Player exposing (..)
import Power exposing (..)


type Page
    = Attacking
    | Defending
    | Results


type Msg
    = PlayerMsg Player.Msg
    | SelectCard (Result String Card)
    | NextPage
    | Reset


type alias Model =
    { attackingPlayer : Player
    , defendingPlayer : Player
    , page : Page
    , selectedCard : Result String Card
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


initialModel : Model
initialModel =
    { attackingPlayer = initPlayer
    , defendingPlayer = initPlayer
    , page = Attacking
    , selectedCard = Ok 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- Reset is the same for any page, just reset the model
        Reset ->
            initialModel

        NextPage ->
            { model | page = updatePage model.page }

        SelectCard card ->
            { model | selectedCard = card }

        PlayerMsg playerMsg ->
            case model.page of
                Attacking ->
                    { model | attackingPlayer = updatePlayer playerMsg model.attackingPlayer }

                Defending ->
                    { model | defendingPlayer = updatePlayer playerMsg model.defendingPlayer }

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
    let
        attackingTotal =
            totalPower attackingPlayer

        defendingtotal =
            totalPower defendingPlayer

        buildText prefix winningPlayer losingPlayer =
            prefix
                ++ " player wins with a total power of "
                ++ (totalPower winningPlayer |> toString)
                ++ " to "
                ++ (totalPower losingPlayer |> toString)
                ++ "!"

        resultText =
            if attackingTotal >= defendingtotal then
                buildText "Attacking" attackingPlayer defendingPlayer
            else
                buildText "Defending" defendingPlayer attackingPlayer
    in
        [ text resultText
        , br [] []
        , resetButton
        ]


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
            totalPower player |> toString
    in
        [ h1 [] [ text titleText ]
        , h2 [] [ text <| "Your current total power is " ++ currentPowerStr ++ "." ]
        , powerSelect player
        , cardSelect
        , cardsListGroup <| getCards player
        , div []
            [ resetButton
            , nextButton nextText
            ]
        ]


{-| <select /> for adding a new card
-}
cardSelect : Html Msg
cardSelect =
    selectp cardPrism SelectCard 0 [ class "form-control" ] cardOptions


{-| <select /> for a player's power
-}
powerSelect : Player -> Html Msg
powerSelect player =
    selectp powerPrism (PlayerMsg << SetPower) (getPower player) [ class "form-control" ] powerOptions


{-| <option />s for the power <select />
-}
powerOptions : List ( String, Power )
powerOptions =
    List.map (\p -> ( powerToLabel p, p )) allPowers


{-| <option />s for the card <select />
-}
cardOptions : List ( String, Card )
cardOptions =
    List.map (\c -> ( cardToLabel c, c )) allCards


cardsListGroup : List Card -> Html Msg
cardsListGroup cards =
    div [ class "form-group" ] [ ul [ class "list-group" ] <| List.indexedMap cardListItem cards ]


cardListItem : Int -> Card -> Html Msg
cardListItem index card =
    li
        [ class "list-group-item justify-content-between"
        ]
        [ text <| cardToLabel card
        , span [ class "float-right" ]
            [ button
                [ type_ "button"
                , class "close"
                , onClick <| PlayerMsg (RemoveCard index)
                ]
                [ span [ class "text-danger" ] [ text "×" ] ]
            ]
        ]


resetButton : Html Msg
resetButton =
    button
        [ onClick Reset
        , class "btn btn-danger"
        , type_ "button"
        ]
        [ text "Reset" ]


nextButton : String -> Html Msg
nextButton string =
    button
        [ onClick NextPage
        , class "btn btn-primary"
        , type_ "button"
        ]
        [ text string ]
