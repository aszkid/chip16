module App exposing (main)
import Chip16 exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import File exposing (File)
import File.Select as Select
import Task
import Bytes exposing (Bytes)

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
  , file : Maybe Bytes
  }

type Msg
  = FileRequested
  | FileLoaded File
  | FileContentLoaded Bytes

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
    [ text ("File = " ++ Debug.toString model.file)
    , br [] []
    , button [ class "btn btn-primary", onClick FileRequested ] [ text "Load ROM" ]
    , br [] []
    , button [ class "btn btn-success" ] [ text "Step" ]
    , button [ class "btn btn-warning" ] [ text "Reset" ]
    , br [] []
    , text ("PC: "
            ++ Debug.toString model.machine.cpu.pc
            ++ ", SP: "
            ++ Debug.toString model.machine.cpu.sp
            ++ ", Flags: "
            ++ Debug.toString model.machine.cpu.flags) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileRequested -> (model, Select.file ["application/octet-stream"] FileLoaded)
    FileLoaded f -> (model, Task.perform FileContentLoaded (File.toBytes f))
    FileContentLoaded b -> ({ model | file = Just b }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none