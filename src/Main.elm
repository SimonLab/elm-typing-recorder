module Main exposing (main)

import Browser
import Html exposing (div, text)
import Html.Events exposing (onClick)


-- Model
type alias Model = Int
type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init _ = (0, Cmd.none)

-- Update

type Msg = Add

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Add -> (model + 1, Cmd.none)


-- Subscription
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


--  View
view : Model -> Browser.Document Msg
view model = 
  { title = "Typing Recorder"
  , body = [
      div [onClick Add] [text <| String.fromInt model]
    ]
  }


main : Program Flags Model Msg
main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }
