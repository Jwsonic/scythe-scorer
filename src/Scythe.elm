module Scythe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, selected, type_, value)
import Html.Events exposing (..)
import Player exposing (..)


type Page
    = Attacking
    | Defending
    | Results


type Msg
    = SetPower Power
    | AddCard Card
    | RemoveCard Int
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
            setPower newPower player

        AddCard newCard ->
            addCard newCard player

        RemoveCard index ->
            removeCard index player

        _ ->
            player


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

        buildText prefix player =
            prefix ++ " player wins with a total power of " ++ (totalPower player |> toString) ++ "!"

        resultText =
            if attackingTotal >= defendingtotal then
                buildText "Attacking" attackingPlayer
            else
                buildText "Defending" defendingPlayer
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
        , selectPower <| getPower player
        , selectCard
        , cardsListGroup <| getCards player
        , div []
            [ resetButton
            , nextButton nextText
            ]
        ]


selectPower : Power -> Html Msg
selectPower power =
    selectForm (optionBuilder power allPower) "power" (stringToPower >> SetPower)


selectCard : Html Msg
selectCard =
    selectForm (optionBuilder 0 allCards) "card" (stringToCard >> AddCard)


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


cardsListGroup : List Card -> Html Msg
cardsListGroup cards =
    ul [ class "list-group" ] <| List.indexedMap cardListItem cards


cardListItem : Int -> Card -> Html Msg
cardListItem index card =
    li
        [ class "list-group-item"
        ]
        [ text <| "Card " ++ (toString card)
        , button
            [ type_ "button"
            , class "close float-right"
            , onClick <| RemoveCard index
            ]
            [ span [ class "text-danger" ] [ text "×" ] ]
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
