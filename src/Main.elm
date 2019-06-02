module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Dom
import Html exposing (..)
import Html.Events exposing (onClick, onFocus, onBlur)
import Html.Attributes exposing (..)
import Json.Decode as JDecode
import Time
import Task
import Process

-- Model

type alias Model = 
  { recorder : Recorder
  , player : Player
  }

initModel : Model
initModel = {recorder = initRecorder, player = initPlayer}


-- Recorder

type alias Recorder = {recording : Bool, hasFocus: Bool, record : List KeyStroke}

type alias KeyStroke = (String, Time.Posix)

initRecorder : Recorder
initRecorder = Recorder False False []


-- Player

type alias Player = {track : String, playerStatus : PlayerStatus}

type PlayerStatus = Playing | Paused | Stopped

initPlayer : Player
initPlayer = Player "" Stopped


-- Init

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)


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

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({recorder, player} as model) = 
  case msg of
    ToggleRecording -> 
      let
        recording = not recorder.recording
      in
        if recording then
          ({model | recorder = { recorder | recording = recording }}, Task.attempt FocusTypeRecorder (Browser.Dom.focus "type-recorder"))
        else
          ({model | recorder = { recorder | recording = recording }}, Cmd.none) 

    FocusTypeRecorder (Err _) -> (model, Cmd.none)

    FocusTypeRecorder (Ok _) -> (model, Cmd.none)

    RecordKeyStroke key -> ( model, Task.perform (KeyStroke key) Time.now)

    KeyStroke key time -> 
      if model.recorder.hasFocus && model.recorder.recording then
        ( {model | recorder = {recorder | record = recorder.record ++ [(key, time)]}}, Cmd.none)
      else
        (model, Cmd.none)

    ResetRecord -> 
      ({model | recorder = {recorder| recording = False, record = []}, player = {track = "", playerStatus = Stopped}}, Cmd.none)

    FocusOnRecorder ->
      ({model | recorder = {recorder | hasFocus = True}}, Cmd.none)

    BlurOnRecorder ->
      ({model | recorder = {recorder | hasFocus = False}}, Cmd.none)

    PlayRecord -> 
      let
        start : Float
        start = case model.recorder.record of
           [] -> 0
           (_, posix) :: _ -> toFloat <| Time.posixToMillis posix
      in
        ( {model | player = {player | playerStatus = Playing }}
        , Cmd.batch
        <| List.map (\(key, posix) -> Task.perform (always <| Play key) <| Process.sleep ((toFloat <| Time.posixToMillis posix) - start))
        <| model.recorder.record
        )


    Play s -> ({model | player = {player | track = player.track ++ s }}, Cmd.none)


-- Subscription
subscriptions : Model -> Sub Msg
subscriptions _ = onKeyDown (JDecode.map RecordKeyStroke keyDecoder)

keyDecoder : JDecode.Decoder String
keyDecoder = JDecode.field "key" JDecode.string


--  View
view : Model -> Browser.Document Msg
view model = 
  { title = "Typing Recorder"
  , body = 
    [ h1 [] [text "Typing Recorder"]
    , button [onClick ToggleRecording] [text <| if model.recorder.recording then "Stop" else "Start"]
    , button [onClick ResetRecord] [text "Reset recording"]
    , button [onClick PlayRecord] [text "Play"]
    , textarea [ id "type-recorder"
               , placeholder "type your text here..."
               , value model.player.track
               , onFocus FocusOnRecorder
               , onBlur BlurOnRecorder
               , disabled (model.player.playerStatus == Playing && not model.recorder.recording)
               ] []
    ]
  }

main : Program Flags Model Msg
main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }
