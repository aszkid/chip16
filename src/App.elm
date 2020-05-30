module App exposing (main)
import Chip16 exposing (..)
import Numbers exposing (Int8, Int16, ChipInt(..), to, i8from, i16from)
import Slice exposing (Slice, get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import File exposing (File)
import File.Select as Select
import Task
import Bytes exposing (Bytes, Endianness (..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Extra as Decode
import Time
import Hex
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Graphics

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
  , tick : Int
  , screen : List (Renderable)
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

looper : Int -> (Int, Slice Int16) -> Decoder (Decode.Step (Int, Slice Int16) (Slice Int16))
looper romsz (idx, slice) = 
  if idx == romsz then
    Decode.succeed (Decode.Done slice)
  else
    Decode.map (\v16 -> Decode.Loop (idx + 2, Slice.set idx (i16from v16) slice)) (Decode.unsignedInt16 LE)

bytesToMemory : Int -> Decoder (Slice Int16)
bytesToMemory romsz = 
  Decode.loop
    (0, Slice.new 65536 (i16from 0)) -- index, LL HH, result
    (looper romsz)

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
  , tick = 0
  , screen = []
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

toHex16 : Int -> String
toHex16 v = 
  let
    out = Hex.toString v
    pre = String.repeat (4 - String.length out) "0"
  in
    pre ++ out

get_rx : Model -> Int -> String
get_rx model i =
  case Slice.get i model.machine.cpu.regs of
    Just val -> "0x" ++ toHex16 (to (I16 val))
    _ -> "0x0000"

regs_table : Model -> Html Msg
regs_table model =
  table [ class "table table-sm" ] [
    tr [] [
      td [] [ b [] [Html.text "R0 "], Html.text (get_rx model 0x0) ],
      td [] [ b [] [Html.text "R8 "], Html.text (get_rx model 0x8) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R1 "], Html.text (get_rx model 0x1) ],
      td [] [ b [] [Html.text "R9 "], Html.text (get_rx model 0x9) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R2 "], Html.text (get_rx model 0x2) ],
      td [] [ b [] [Html.text "RA "], Html.text (get_rx model 0xA) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R3 "], Html.text (get_rx model 0x3) ],
      td [] [ b [] [Html.text "RB "], Html.text (get_rx model 0xB) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R4 "], Html.text (get_rx model 0x4) ],
      td [] [ b [] [Html.text "RC "], Html.text (get_rx model 0xC) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R5 "], Html.text (get_rx model 0x5) ],
      td [] [ b [] [Html.text "RD "], Html.text (get_rx model 0xD) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R6 "], Html.text (get_rx model 0x6) ],
      td [] [ b [] [Html.text "RE "], Html.text (get_rx model 0xE) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R7 "], Html.text (get_rx model 0x7) ],
      td [] [ b [] [Html.text "RF "], Html.text (get_rx model 0xF) ]
    ]
  ]

controls : Model -> Html Msg
controls model =
  div [id "controls"] [
    div [class "btn-toolbar"] [
      div [class "btn-group mr-2"] [
        button [ type_ "button", class "btn btn-primary", onClick FileRequested ] [ Html.text "Load ROM" ]
        , button [ type_ "button", class "btn btn-warning", onClick Reset ] [ Html.text "Reset" ]
      ]
      , div [class "btn-group"] [
        button [ type_ "button", class "btn btn-success", onClick (Step 1 True) ] [ Html.text "Step" ]
        , button [ type_ "button", class "btn btn-secondary", onClick (Running True) ] [ Html.text "Start" ]
        , button [ type_ "button", class "btn btn-danger", onClick (Running False) ] [ Html.text "Pause" ]
      ]
    ]
    , div [id "info"] [
      Html.text ("File = " ++ Debug.toString model.file)
      , br [] []
      , Html.text ("Running: " ++ Debug.toString model.running ++ " | Tick: " ++ Debug.toString model.tick)
      , br [] []
      , Html.text ("Flags: " ++ Debug.toString model.machine.cpu.flags)
      , br [] []
      , Html.text ("Render commands: " ++ Debug.toString (List.length model.machine.graphics.cmdbuffer))
      , br [] []
      , Html.text ("Graphics state: " ++ Debug.toString model.machine.graphics)
    ]
  ]

inspector : Model -> Html Msg
inspector model =
  div [id "inspector"] [
    Html.text ("Instruction = " ++ toStr (prefetch model))
    , br [] []
    , table [class "table table-sm"] [
      tr [] [
        td [] [b [] [Html.text "PC "], Html.text ("0x" ++ toHex16 (to (U16 model.machine.cpu.pc)))]
      ],
      tr [] [
        td [] [b [] [Html.text "SP "], Html.text ("0x" ++ toHex16 (to (U16 model.machine.cpu.sp)))]
      ]
    ]
    , regs_table model
  ]

screen : Model -> Html Msg
screen model =
  let
    width = 640
    height = 480
  in
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml (width, height)
            [ style "border" "1px solid black" ]
            --(List.concat [ [shapes [ fill Color.white ] [ rect (0, 0) width height ]], render model ])
            (render model)
        ]
render : Model -> List (Renderable)
render model = Graphics.produce model.machine.graphics.cmdbuffer model.machine.graphics.palette

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [Html.text "elm16"]
    , div [class "d-flex flex-row"] [
        div [id "screen"] [ screen model ],
        inspector model
    ]
    , controls model
    ]

take_step : Model -> Model
take_step model =
  let
    should_vblank = model.tick >= 16666
  in
    case prefetch model of
      Instruction a b c d ->
        case Debug.log "Instruction: " (Hex.toString a ++ " " ++ Hex.toString b) of
        _ -> { model
          | machine = dispatch model.machine should_vblank (i8from a) (i8from b) (i8from c) (i8from d)
          , tick = if should_vblank then 0 else model.tick + 1
          , screen = if should_vblank then List.concat [ [shapes [ fill Color.white ] [ rect (0, 0) 640 480 ]], render model ] else model.screen }

steps : Int -> Model -> Model
steps n model =
  let
    take_steps : Bytes -> Model
    take_steps rom =
      List.foldl
        (\_ the_model -> take_step the_model)
        model
        (List.range 1 n)
  in
    case model.rom of
      Nothing -> model
      Just rom ->
        let
          out_model = take_steps rom
        in 
          out_model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileRequested -> (model, Select.file ["application/octet-stream"] FileLoaded)
    FileLoaded f -> (model, Task.perform FileContentLoaded (File.toBytes f))
    FileContentLoaded b ->
      let
        hdr = Decode.decode headerDecoder b
        rom = case hdr of
          Just h -> Decode.decode (romDecoder h) b
          Nothing -> Nothing
      in (
        { model
        | file = Just b
        , hdr = hdr
        , machine = case (hdr, rom) of
            (Just theHdr, Just theRom) -> case Decode.decode (bytesToMemory theHdr.romsz) theRom of
              Just memslice -> Chip16.initFrom memslice
              _ -> Chip16.init
            _ -> Chip16.init
        , tick = 0
        , rom = rom}
        , Cmd.none
      )
    Step n force -> if model.running || force then (steps n model, Cmd.none) else (model, Cmd.none)
    Running b -> ({ model | running = b }, Cmd.none)
    Reset -> ({ model | machine = Chip16.init, tick = 0 }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 60 (\t -> Step 10000 False)