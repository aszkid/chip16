module App exposing (main)
import Chip16 exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser

main = 
  Browser.sandbox { init = initModel, view = view, update = update }

type alias Model = 
  { machine : Chip16 }

type Msg = LoadExecPrompt

initModel : Model
initModel =
  { machine = Chip16.init }

view : Model -> Html Msg
view model =
  div []
    [ text ("PC=" ++ Debug.toString model.machine.cpu.pc),
      button [ onClick LoadExecPrompt ] [ text "file" ] ]

update : Msg -> Model -> Model
update msg model = model