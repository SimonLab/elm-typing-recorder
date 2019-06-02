module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (placeholder)
import Json.Decode as JDecode
import Time
import Task
import Process

-- Model
type alias Model = 
  { status : Status
  , recording : String
  , buffer : List Key
  }

type alias Key = (String, Time.Posix)


type Status = Recording | Playing | None

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)


initModel : Model
initModel = {status = None, recording = "", buffer = []}

-- Update

type Msg = ToggleRecording | RecordKey String | ResetBuffer | KeyTime String Time.Posix | Play | PlayRecording String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    ToggleRecording -> ({model | status = toggleStatus model.status}, Cmd.none)

    RecordKey key -> (model, Task.perform (KeyTime key) Time.now)

    KeyTime key time -> ({model | buffer = model.buffer ++ [(key, time)]}, Cmd.none)

    ResetBuffer -> ({model | buffer = []}, Cmd.none)

    Play -> 
      let
        start : Float
        start = case model.buffer of
           [] -> 0
           (_, posix) :: _ -> toFloat <| Time.posixToMillis posix
      in
      ( model
      , Cmd.batch
      <| List.map (\(key, posix) -> Task.perform (always <| PlayRecording key) <| Process.sleep ((toFloat <| Time.posixToMillis posix) - start))
      <| model.buffer
      )


    PlayRecording s -> ({model | recording = model.recording ++ s }, Cmd.none)


toggleStatus : Status -> Status
toggleStatus status =
  case status of
    Recording -> None
    None -> Recording
    _ -> status

-- Subscription
subscriptions : Model -> Sub Msg
subscriptions _ = onKeyDown (JDecode.map RecordKey keyDecoder)

keyDecoder : JDecode.Decoder String
keyDecoder = JDecode.field "key" JDecode.string


--  View
view : Model -> Browser.Document Msg
view model = 
  { title = "Typing Recorder"
  , body = 
    [ h1 [] [text "Typing Recorder"]
    , button [onClick ToggleRecording] [text <| if model.status == Recording then "Stop" else "Start recording"]
    , button [onClick ResetBuffer] [text "Reset recording"]
    , button [onClick Play] [text "Play"]
    , textarea [placeholder "type your text here..."] []
    , displayBuffer model.buffer
    , div [] [text model.recording]
    ]
  }

displayBuffer : List Key -> Html Msg
displayBuffer buffer =
  div [] [text <| String.concat <| List.map Tuple.first buffer]


main : Program Flags Model Msg
main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }
