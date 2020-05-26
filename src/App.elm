module App exposing (main)
import Chip16 exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import File exposing (File)
import File.Select as Select
import Task
import Bytes exposing (Bytes, Endianness (..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Extra
import Bytes.Decode.Extra as Decode
import Time

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
  , time : Time.Posix
  , delta : Int
  , hdr : Maybe Header
  , rom : Maybe Bytes
  }

type alias Header =
  { magic : String
  , specv : Int
  , romsz : Int
  , start : Int
  , checksum : Int }

headerInit : Header
headerInit =
  { magic = ""
  , specv = 0
  , romsz = 0
  , start = 0
  , checksum = 0 }

headerDecoder : Decoder Header
headerDecoder =
  let
    stepfn (step, hdr) = case step of
      0 -> Decode.map (\m -> Decode.Loop (1, { hdr | magic = m })) (Decode.string 4)
      1 -> Decode.map (\sv -> Decode.Loop (2, { hdr | specv = sv })) (Decode.withOffset 1 Decode.unsignedInt8)
      2 -> Decode.map (\sz -> Decode.Loop (3, { hdr | romsz = sz })) (Decode.unsignedInt32 LE)
      3 -> Decode.map (\st -> Decode.Loop (4, { hdr | start = st })) (Decode.unsignedInt16 LE)
      4 -> Decode.map (\ck -> Decode.Loop (5, { hdr | checksum = ck})) (Decode.unsignedInt32 LE)
      5 -> Decode.succeed (Decode.Done hdr)
      _ -> Debug.todo "invalid decoder state"
  in
    Decode.loop (0, headerInit) stepfn

romDecoder : Header -> Decoder Bytes
romDecoder hdr =
  Decode.withOffset 16 (Decode.bytes hdr.romsz)

type Msg
  = FileRequested
  | FileLoaded File
  | FileContentLoaded Bytes
  | FrameBegin Time.Posix
  | FrameEnd Time.Posix

type alias Flags = ()

initModel : Model
initModel =
  { machine = Chip16.init
  , file = Nothing
  , time = Time.millisToPosix 0
  , delta = 0
  , hdr = Nothing
  , rom = Nothing
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
    , text ("frametime: " ++ Debug.toString model.delta)
    , br [] []
    , text ("PC: "
            ++ Debug.toString model.machine.cpu.pc
            ++ ", SP: "
            ++ Debug.toString model.machine.cpu.sp
            ++ ", Flags: "
            ++ Debug.toString model.machine.cpu.flags)
    , br [] []
    , text ("Header: " ++ Debug.toString model.hdr )
    , br [] []
    , text ("Rom: " ++ Debug.toString model.rom ) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileRequested -> (model, Select.file ["application/octet-stream"] FileLoaded)
    FileLoaded f -> (model, Task.perform FileContentLoaded (File.toBytes f))
    FileContentLoaded b ->
      let
        hdr = Decode.decode headerDecoder b
      in (
        { model
        | file = Just b
        , hdr = hdr
        , rom = case hdr of
            Just h -> Decode.decode (romDecoder h) b
            Nothing -> Nothing }
        , Cmd.none
      )
    FrameBegin t -> ({ model | time = t }, Task.perform FrameEnd Time.now)
    FrameEnd t -> ({ model | delta = Time.posixToMillis t - Time.posixToMillis model.time}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none