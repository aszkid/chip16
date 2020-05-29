module App exposing (main)
import Chip16 exposing (..)
import Numbers exposing (Int8, ChipInt(..), to, i8from)
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
import Hex

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
  , running : Bool
  , instruction : String
  , tick : Int
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

type Instruction = Instruction Int Int Int Int
instructionDecoder : Int -> Decoder Instruction
instructionDecoder pc =
  Decode.withOffset pc (
    Decode.loop (0, Instruction 0 0 0 0) (\(step, Instruction a b c d) -> 
      if step >= 4 then
        Decode.succeed (Decode.Done (Instruction a b c d))
      else
        Decode.map
          (\byte -> case step of
            0 -> Decode.Loop (1, Instruction byte 0 0 0)
            1 -> Decode.Loop (2, Instruction a byte 0 0)
            2 -> Decode.Loop (3, Instruction a b byte 0)
            3 -> Decode.Loop (4, Instruction a b c byte)
            _ -> Debug.todo "failed to decode isntruction")
          Decode.unsignedInt8))

type Msg
  = FileRequested
  | FileLoaded File
  | FileContentLoaded Bytes
  | Step Int Bool
  | Running Bool
  | Reset

type alias Flags = ()

initModel : Model
initModel =
  { machine = Chip16.init
  , file = Nothing
  , time = Time.millisToPosix 0
  , delta = 0
  , hdr = Nothing
  , rom = Nothing
  , running = False
  , instruction = ""
  , tick = 0
  }

init : Flags -> (Model, Cmd Msg)
init () =
    (initModel, Cmd.none)

prefetch : Model -> Instruction
prefetch model =
  case model.rom of
    Nothing -> Instruction 0 0 0 0 -- not going to happen
    Just rom ->
      case Decode.decode (instructionDecoder (to (U16 model.machine.cpu.pc))) rom of
        Just instr -> instr
        _ -> Instruction 0 0 0 0

toStr : Instruction -> String
toStr (Instruction a b c d) =
  Hex.toString a ++ " " ++ Hex.toString b ++ " " ++ Hex.toString c ++ " " ++ Hex.toString d

view : Model -> Html Msg
view model =
  div []
    [ text ("File = " ++ Debug.toString model.file)
    , br [] []
    , text ("Instruction = " ++ toStr (prefetch model))
    , br [] []
    , button [ class "btn btn-primary", onClick FileRequested ] [ text "Load ROM" ]
    , button [ class "btn btn-warning", onClick Reset ] [ text "Reset" ]
    , br [] []
    , button [ class "btn btn-success", onClick (Step 1 True) ] [ text "Step" ]
    , button [ class "btn btn-secondary", onClick (Running True) ] [ text "Start" ]
    , button [ class "btn btn-danger", onClick (Running False) ] [ text "Pause" ]
    , br [] []
    , text ("Running: " ++ Debug.toString model.running)-- ++ " | Tick: " ++ Debug.toString model.tick)
    , br [] []
    , text ("PC: "
            ++ Hex.toString (to (U16 model.machine.cpu.pc))
            ++ ", SP: "
            ++ Hex.toString (to (U16 model.machine.cpu.sp))
            ++ ", Flags: "
            ++ Debug.toString model.machine.cpu.flags)
    , br [] []
    , text ("Header: " ++ Debug.toString model.hdr )
    , br [] []
    , text ("Rom: " ++ Debug.toString model.rom ) ]

take_step : Model -> Bytes -> (Model, Instruction)
take_step model rom =
  let
    should_vblank = model.tick >= 16666
  in
    case prefetch model of
      Instruction a b c d ->
        ( { model
          | machine = dispatch model.machine should_vblank (i8from a) (i8from b) (i8from c) (i8from d)
          , tick = if should_vblank then 0 else model.tick + 1 }
        , Instruction a b c d
        )

steps : Int -> Model -> Model
steps n model =
  let
    take_steps : Bytes -> (Model, Instruction)
    take_steps rom =
      List.foldl
        (\_ (the_model, _) -> take_step the_model rom)
        (model, Instruction 0 0 0 0)
        (List.range 1 n)
  in
    case model.rom of
      Nothing -> model
      Just rom ->
        let
          (out_model, Instruction a b c d) = take_steps rom
        in 
          { out_model
          | instruction = Hex.toString a ++ " " ++ Hex.toString b ++ " " ++ Hex.toString c ++ " " ++ Hex.toString d
          }

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
    Step n force -> if model.running || force then (steps n model, Cmd.none) else (model, Cmd.none)
    Running b -> ({ model | running = b }, Cmd.none)
    Reset -> ({ model | machine = Chip16.init, tick = 0 }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 60 (\t -> Step 10000 False)