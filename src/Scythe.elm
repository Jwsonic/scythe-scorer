module Scythe exposing (..)

import Card exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, for, id, selected, style, type_, value)
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
    , selectedCard = Ok 2
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
                    attackingPlayerView model.selectedCard model.attackingPlayer

                Defending ->
                    defendingPlayerView model.selectedCard model.defendingPlayer

                Results ->
                    resultsView model
    in
        div [ class "container" ] body


{-| handles the drawing of the Results Page
-}
resultsView : Model -> List (Html Msg)
resultsView { attackingPlayer, defendingPlayer } =
    let
        attackingTotal =
            totalPower attackingPlayer

        defendingtotal =
            totalPower defendingPlayer

        buildText prefix =
            prefix ++ " player wins!"

        resultText =
            if attackingTotal >= defendingtotal then
                buildText "Attacking"
            else
                buildText "Defending"

        ( attackingTitle, attackingStats ) =
            playerStats "Attacking Player" attackingPlayer

        ( defendingTitle, defendingStats ) =
            playerStats "Defending Player" defendingPlayer
    in
        [ h1 [ class "title has-text-centered" ] [ text resultText ]
        , hr [] []
        , attackingTitle
        , attackingStats
        , hr [] []
        , defendingTitle
        , defendingStats
        , hr [] []
        , div [ class "has-text-centered" ] [ resetButton ]
        ]


attackingPlayerView : Result String Card -> Player -> List (Html Msg)
attackingPlayerView =
    playerView "Attacking Turn" "Next Turn"


defendingPlayerView : Result String Card -> Player -> List (Html Msg)
defendingPlayerView =
    playerView "Defending Turn" "View results"


{-| playerView handles drawing the player agnostic view for the current player
-}
playerView : String -> String -> Result String Card -> Player -> List (Html Msg)
playerView titleText nextText selectedCard player =
    let
        cards =
            getCards player

        ( statsTitle, stats ) =
            playerStats titleText player
    in
        [ statsTitle
        , stats
        , powerSelect player
        , cardSelectForm selectedCard
        , cardsListGroup cards
        , buttonGroup nextText
        ]


playerStats : String -> Player -> ( Html Msg, Html Msg )
playerStats title player =
    let
        totalPowerStr =
            player |> totalPower |> toString

        cardStr =
            player |> getCards |> List.sum |> toString

        powerStr =
            player |> getPower |> toString
    in
        ( h1 [ class "title has-text-centered" ] [ text title ]
        , nav [ class "level is-mobile" ]
            [ levelItem "Total Power" totalPowerStr
            , levelItem "Cards" cardStr
            , levelItem "Power" powerStr
            ]
        )


levelItem : String -> String -> Html Msg
levelItem heading title =
    div [ class "level-item has-text-centered" ]
        [ div []
            [ p [ class "heading" ] [ text heading ]
            , p [ class "title" ] [ text title ]
            ]
        ]


{-| <select /> for adding a new card
-}
cardSelectForm : Result String Card -> Html Msg
cardSelectForm selectedCard =
    let
        card =
            Result.withDefault 2 selectedCard

        onAddCardClick =
            onClick <| PlayerMsg <| AddCard selectedCard
    in
        div [ class "field has-addons" ]
            [ p [ class "control is-expanded" ]
                [ span [ class "select is-fullwidth" ]
                    [ selectp cardPrism SelectCard card [ class "form-control" ] cardOptions
                    ]
                ]
            , p
                [ class "control is-primary" ]
                [ button
                    [ class "button is-primary"
                    , onAddCardClick
                    ]
                    [ text "Add Card" ]
                ]
            ]


{-| <select /> for a player's power
-}
powerSelect : Player -> Html Msg
powerSelect player =
    div [ class "field" ]
        [ span [ class "select is-fullwidth" ]
            [ selectp powerPrism (PlayerMsg << SetPower) (getPower player) [ class "form-control" ] powerOptions
            ]
        ]


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
    div [ class "field" ]
        [ div [ class "columns" ] [ div [ class "column" ] <| List.indexedMap cardListItem cards ]
        ]


cardListItem : Int -> Card -> Html Msg
cardListItem index card =
    div
        [ class "notification is-fullwidth"
        ]
        [ button
            [ class "delete"
            , onClick <| PlayerMsg (RemoveCard index)
            ]
            []
        , text <| cardToLabel card
        ]


buttonGroup : String -> Html Msg
buttonGroup nextText =
    div [ class "has-text-centered" ]
        [ resetButton
        , button
            [ onClick NextPage
            , class "button is-primary"
            ]
            [ text nextText ]
        ]


resetButton : Html Msg
resetButton =
    button
        [ onClick Reset
        , class "button is-danger"
        ]
        [ text "Reset" ]
