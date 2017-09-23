module NewUI exposing (main)

import Html exposing (Html, div, img)
import Html.Attributes exposing (id, src, style)
import Html.Events exposing (on)
import Json.Decode as Json exposing (andThen, float)
import Json.Decode.Pipeline exposing (decode, required, requiredAt)
import Time exposing (Time)
import AnimationFrame exposing (times)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Raw CoordianteData about a touch/click event. Contains width/height as well for local/polar
coordiante calculation.
-}
type alias CoordinateData =
    { x : Float
    , y : Float
    , height : Float
    , width : Float
    }


{-| Represents a Polar Coordinate within the wheel. Json decoders guarentee that if
you get a PolarCoordiante in a Msg, it is in the wheel.
-}
type alias PolarCoordiante =
    { distance : Float
    , angle : Float
    }


type alias Model =
    { rotation : Float {- The current rotation to use -}
    , nextRotation : Float {- The next rotation to use when RAF syncs -}
    , lastTouch : PolarCoordiante {- The last coordinate touched -}
    }


type Msg
    = WheelClick PolarCoordiante
    | TouchStart PolarCoordiante
    | TouchMove PolarCoordiante
    | TouchEnd PolarCoordiante
    | TouchCancel PolarCoordiante
    | AnimFrame Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WheelClick data ->
            -- Save the next rotation where the user clicked
            ( { model
                | nextRotation = calculateClickRotation model.nextRotation data
              }
            , Cmd.none
            )

        TouchStart touch ->
            -- Save the the start so we can calculate diffs
            ( { model | lastTouch = touch }, Cmd.none )

        TouchMove touch ->
            -- Save the next rotation diff
            ( { model
                | lastTouch = touch
                , nextRotation = calculateTouchRotation model.rotation model.lastTouch touch
              }
            , Cmd.none
            )

        TouchEnd touch ->
            -- Save the next rotation diff
            ( { model
                | lastTouch = touch
                , nextRotation = calculateTouchRotation model.rotation model.lastTouch touch |> snapToPoint
              }
            , Cmd.none
            )

        AnimFrame _ ->
            -- Update the current rotation since we're in the RAF frame
            ( { model | rotation = model.nextRotation }, Cmd.none )

        TouchCancel _ ->
            -- Ignore this for now
            ( model, Cmd.none )


view : Model -> Html Msg
view { rotation } =
    div []
        [ wheelImg rotation
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    times AnimFrame


init : ( Model, Cmd Msg )
init =
    ( { rotation = 0, nextRotation = 0, lastTouch = { distance = 0, angle = 0 } }, Cmd.none )


wheelImg : Float -> Html Msg
wheelImg rotation =
    let
        wheelStyle =
            style
                [ ( "transform", "rotate(" ++ toString rotation ++ "rad)" )
                , ( "touch-action", "none" ) {- Prevents pull to refresh on mobile -}
                ]
    in
        img
            [ src "img/power_dial_1.png"
            , on "click" decodeClick
            , on "touchstart" <| decodeTouchEvent TouchStart
            , on "touchmove" <| decodeTouchEvent TouchMove
            , on "touchend" <| decodeTouchEvent TouchEnd
            , on "touchcancel" <| decodeTouchEvent TouchCancel
            , id "wheel"
            , wheelStyle
            ]
            []


{-| decodeTouchEvent decodes a single touch event into a polar coordiante
-}
decodeTouchEvent : (PolarCoordiante -> value) -> Json.Decoder value
decodeTouchEvent event =
    decode CoordinateData
        |> requiredAt [ "touches", "0", "clientX" ] float
        |> requiredAt [ "touches", "0", "clientY" ] float
        |> decodeWidthHeight
        |> andThen decodeInWheel
        |> Json.map event


decodeClick : Json.Decoder Msg
decodeClick =
    decode CoordinateData
        |> required "clientX" float
        |> required "clientY" float
        |> decodeWidthHeight
        |> andThen decodeInWheel
        |> Json.map WheelClick


decodeWidthHeight : Json.Decoder (Float -> Float -> a) -> Json.Decoder a
decodeWidthHeight decoder =
    decoder
        |> requiredAt [ "target", "height" ] float
        |> requiredAt [ "target", "width" ] float


{-|
    decodeInWheel turns a CoordinateData into a PolarCoordiante if it exists in the wheel.
-}
decodeInWheel : CoordinateData -> Json.Decoder PolarCoordiante
decodeInWheel { x, y, width, height } =
    let
        localX =
            (width / 2 - x) * -1

        localY =
            height / 2 - y

        ( distance, angle ) =
            toPolar ( localX, localY )

        wheelRadius =
            width / 2
    in
        if distance > wheelRadius then
            Json.fail "Not in wheel"
        else
            Json.succeed { distance = distance, angle = angle }


{-| calculateClickRotation calculates the new angle for a click
-}
calculateClickRotation : Float -> PolarCoordiante -> Float
calculateClickRotation currentRotation { angle } =
    Debug.log "click" <| currentRotation + angle - pi / 2


{-| calculateTouchRotation calculates the new angle for a touch
-}
calculateTouchRotation : Float -> PolarCoordiante -> PolarCoordiante -> Float
calculateTouchRotation currentRotation lastTouch touch =
    currentRotation + lastTouch.angle - touch.angle


pointAngles : List Float
pointAngles =
    List.range 0 7
        |> List.map toFloat
        |> List.map ((*) (pi / 8))
        |> List.map radians


snapToPoint : Float -> Float
snapToPoint angle =
    let
        absAngle = abs <| Debug.log "old angle" angle

        chooseCloser nextAngle currentAngle =
            if abs (currentAngle - absAngle) < abs (nextAngle - absAngle) then
                currentAngle
            else
                nextAngle
    in
        List.foldr chooseCloser (10 * pi) pointAngles
