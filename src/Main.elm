module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onFocus, onMouseDown)
import Json.Decode as JDecode
import Process
import Task
import Time



-- Model


type alias Model =
    { recorder : Recorder
    , player : Player
    }


initModel : Model
initModel =
    { recorder = initRecorder, player = initPlayer }



-- Recorder


type alias Recorder =
    { hasFocus : Bool, record : List KeyStroke, preview : String }


type alias KeyStroke =
    ( String, Time.Posix )


initRecorder : Recorder
initRecorder =
    Recorder False [] ""



-- Player


type alias Player =
    { track : String, playerStatus : PlayerStatus }


type PlayerStatus
    = Playing
    | Stopped
    | Recording


initPlayer : Player
initPlayer =
    Player "" Stopped



-- Init


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- Update


type Msg
    = ToggleRecording
    | RecordKeyStroke String
    | ResetRecord
    | KeyStroke String Time.Posix
    | PlayRecord
    | Play String
    | FocusTypeRecorder (Result Browser.Dom.Error ())
    | FocusOnRecorder
    | BlurOnRecorder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ recorder, player } as model) =
    case msg of
        ToggleRecording ->
                  case model.player.playerStatus of
                                  Stopped -> ( { model | player = { player | playerStatus = Recording } }, Task.attempt FocusTypeRecorder (Browser.Dom.focus "type-recorder") )
                                  Recording -> ( { model | player = { player | playerStatus = Stopped } }, Cmd.none)
                                  _ -> (model, Cmd.none)

        FocusTypeRecorder (Err _) ->
            ( model, Cmd.none )

        FocusTypeRecorder (Ok _) ->
            ( model, Cmd.none )

        RecordKeyStroke key ->
            ( model, Task.perform (KeyStroke key) Time.now )

        KeyStroke key time ->
            if model.recorder.hasFocus && model.player.playerStatus == Recording then
                ( { model | recorder = { recorder | record = recorder.record ++ [ ( key, time )], preview = recorder.preview ++ key  } }, Cmd.none )

            else
                ( model, Cmd.none )

        ResetRecord ->
            ( { model | recorder = { recorder | record = [], preview = "" }, player = { track = "", playerStatus = Stopped } }, Cmd.none )

        FocusOnRecorder ->
            ( { model | recorder = { recorder | hasFocus = True } }, Cmd.none )

        BlurOnRecorder ->
            ( { model | recorder = { recorder | hasFocus = False }, player = {player | playerStatus = Stopped }}, Cmd.none )

        PlayRecord ->
            let
                start : Float
                start =
                    case model.recorder.record of
                        [] ->
                            0

                        ( _, posix ) :: _ ->
                            toFloat <| Time.posixToMillis posix
            in
            ( { model | player = { player | playerStatus = Playing, track = "" } }
            , Cmd.batch <|
                List.map (\( key, posix ) -> Task.perform (always <| Play key) <| Process.sleep ((toFloat <| Time.posixToMillis posix) - start)) <|
                    model.recorder.record
            )

        Play s ->
          let
            track = player.track ++ s
            playerStatus = if track == model.recorder.preview then Stopped else Playing
          in
            ( { model | player = { player | track = track, playerStatus = playerStatus} }, Cmd.none )



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (JDecode.map RecordKeyStroke keyDecoder)


keyDecoder : JDecode.Decoder String
keyDecoder =
    JDecode.field "key" JDecode.string



--  View


view : Model -> Browser.Document Msg
view model =
    { title = "Typing Recorder"
    , body =
        [ h1 [] [ text "Typing Recorder" ]
        , button [ onMouseDown ToggleRecording, disabled (not (recordable model || modelt.player.playerStatus == Recording)) ]
            [ text <|
                if model.player.playerStatus == Recording then
                    "Stop"

                else
                    "Start"
            ]
        , button [ onClick ResetRecord, disabled (disableReset model) ] [ text "Reset recording" ]
        , button [ onClick PlayRecord, disabled (not (playable model)) ] [ text "Play" ]
        , textarea
            [ id "type-recorder"
            , placeholder "type your text here..."
            , value <| if model.player.playerStatus == Playing then  model.player.track else model.recorder.preview
            , onFocus FocusOnRecorder
            , onBlur BlurOnRecorder
            , disabled (model.player.playerStatus /= Recording)
            ]
            []
        ]
    }

disableReset : Model -> Bool
disableReset model =
 not ( model.player.playerStatus == Stopped && (not <| List.isEmpty model.recorder.record))

playable : Model -> Bool
playable model = 
  model.player.playerStatus == Stopped && (not <| List.isEmpty model.recorder.record)

recordable : Model -> Bool
recordable model = 
  model.player.playerStatus == Stopped && List.isEmpty model.recorder.record


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
