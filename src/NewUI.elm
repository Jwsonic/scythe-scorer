module NewUI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (on)
import Json.Decode exposing (float, map)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { clickX: Float,
      clickY: Float
    }


type Msg = NoOP | SpinnerClick Point


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SpinnerClick {clientX, clientY} ->
          ({clickX = (Debug.log "X " clientX), clickY = (Debug.log "Y " clientY)}, Cmd.none)
        NoOP ->
          (model, Cmd.none)
            


view : Model -> Html Msg
view model =
    div []
        [ spinnerImg
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    ({clickX = 0, clickY = 0}, Cmd.none)


spinnerImg : Html Msg
spinnerImg = img [src "img/power_dial_1.png", on "click" decodePoint] []
  
type alias Point = {clientX: Float, clientY: Float}

decodePoint : Json.Decode.Decoder Msg
decodePoint = 
  decode Point 
  |> required "clientX" float
  |> required "clientY" float
  |> Json.Decode.map SpinnerClick