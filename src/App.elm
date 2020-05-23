module App exposing (main)
import Chip16 exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import File exposing (File)
import File.Select as Select
import Task

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model = 
  { machine : Chip16
  , file : Maybe String
  }

type Msg
  = FileRequested
  | FileLoaded File
  | FileContentLoaded String

type alias Flags = ()

initModel : Model
initModel =
  { machine = Chip16.init
  , file = Nothing
  }

init : Flags -> (Model, Cmd Msg)
init () =
    (initModel, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ text ("File = " ++ Debug.toString model.file),
      br [] [],
      button [ class "btn btn-primary", onClick FileRequested ] [ text "Load ROM" ] ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileRequested -> (model, Select.file ["application/octet-stream"] FileLoaded)
    FileLoaded f -> (model, Task.perform FileContentLoaded (File.toString f))
    FileContentLoaded s -> ({ model | file = Just s }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none