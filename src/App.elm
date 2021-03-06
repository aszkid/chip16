module App exposing (main)
import Chip16 exposing (Chip16)
import Numbers2 as Numbers exposing (Number, I16, u16from, i16from, i8from, to, bits)
import Html exposing (Html, table, tr, td, b, div, button, br, h1)
import Html.Attributes exposing (id, class, type_, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Browser
import File exposing (File)
import File.Select as Select
import Task
import Bytes exposing (Bytes, Endianness (..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Decode.Extra as Decode
import Time
import Hex
import Graphics
import Memory exposing (Memory)
import Keyboard exposing (Key)
import Canvas exposing (shapes, rect, clear)
import Canvas.Settings exposing (fill)
import Array

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
  , running : Bool
  , tick : Int
  , pressedKeys : List Key
  , screen : Html Msg
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

bytesToMemory : Int -> Decoder Memory
bytesToMemory romsz = 
  let
    looper : (Int, Memory) -> Decoder (Decode.Step (Int, Memory) (Memory))
    looper (idx, mem) = 
      if idx == romsz then
        Decode.succeed (Decode.Done mem)
      else
        Decode.map
          (\v8 -> Decode.Loop (idx + 1, Memory.set (u16from idx) (i16from v8) mem))
          Decode.unsignedInt8
  in
    Decode.loop
      (0, Memory.init)
      looper

type Msg
  = FileRequested
  | FileLoaded File
  | FileContentLoaded Bytes
  | Step Int Bool Bool
  | Running Bool
  | Reset
  | KeyMsg Keyboard.Msg
  | Render

type alias Flags = ()

initModel : Model
initModel =
  { machine = Chip16.init
  , file = Nothing
  , time = Time.millisToPosix 0
  , delta = 0
  , running = False
  , tick = 0
  , pressedKeys = []
  , screen = Html.div [] []
  }

init : Flags -> (Model, Cmd Msg)
init () =
    (initModel, Cmd.none)

prefetch : Model -> Instruction
prefetch model = 
  let
    ab = case Memory.get model.machine.cpu.pc model.machine.memory of
      Just v -> v
      _ -> i16from 0
    cd = case Memory.get (Numbers.add (u16from 2) model.machine.cpu.pc) model.machine.memory of
      Just v -> v
      _ -> i16from 0
    (a, b) = Numbers.unpacki16 ab
    (c, d) = Numbers.unpacki16 cd
  in
    Instruction (to a) (to b) (to c) (to d)

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
  case Array.get i model.machine.cpu.regs of
    Just val -> "0x" ++ toHex16 (bits val)
    _ -> "0x0000"

regs_table : Model -> Html Msg
regs_table model =
  table [ class "table table-sm" ] [
    tr [] [
      td [] [ b [] [Html.text "R0 "], Html.text (get_rx model 0x0) ],
      td [] [ b [] [Html.text "R4 "], Html.text (get_rx model 0x4) ],
      td [] [ b [] [Html.text "R8 "], Html.text (get_rx model 0x8) ],
      td [] [ b [] [Html.text "RC "], Html.text (get_rx model 0xC) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R1 "], Html.text (get_rx model 0x1) ],
      td [] [ b [] [Html.text "R5 "], Html.text (get_rx model 0x5) ],
      td [] [ b [] [Html.text "R9 "], Html.text (get_rx model 0x9) ],
      td [] [ b [] [Html.text "RD "], Html.text (get_rx model 0xD) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R2 "], Html.text (get_rx model 0x2) ],
      td [] [ b [] [Html.text "R6 "], Html.text (get_rx model 0x6) ],
      td [] [ b [] [Html.text "RA "], Html.text (get_rx model 0xA) ],
      td [] [ b [] [Html.text "RE "], Html.text (get_rx model 0xE) ]
    ],
    tr [] [
      td [] [ b [] [Html.text "R3 "], Html.text (get_rx model 0x3) ],
      td [] [ b [] [Html.text "R7 "], Html.text (get_rx model 0x7) ],
      td [] [ b [] [Html.text "RB "], Html.text (get_rx model 0xB) ],
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
        button [ type_ "button", class "btn btn-success", onClick (Step 1 True False) ] [ Html.text "Step" ]
        , button [ type_ "button", class "btn btn-secondary", onClick (Running True) ] [ Html.text "Start" ]
        , button [ type_ "button", class "btn btn-danger", onClick (Running False) ] [ Html.text "Pause" ]
      ]
    ]
    {--, div [id "info"] [
      Html.text ("File = " ++ Debug.toString model.file)
      , br [] []
      , Html.text ("Running: " ++ Debug.toString model.running ++ " | Tick: " ++ Debug.toString model.tick)
      , br [] []
      , Html.text ("Flags: " ++ Debug.toString model.machine.cpu.flags)
      , br [] []
      , Html.text ("Render commands: " ++ Debug.toString (List.length model.machine.graphics.cmds))
    ]--}
  ]

inspector : Model -> Html Msg
inspector model =
  div [id "inspector", class "flex-fill"] [
    div [] [
      Html.text ("Instruction = " ++ toStr (prefetch model))
      , br [] []
      , table [class "table table-sm"] [
        tr [] [
          td [] [b [] [Html.text "PC "], Html.text ("0x" ++ toHex16 (to model.machine.cpu.pc))]
        ],
        tr [] [
          td [] [b [] [Html.text "SP "], Html.text ("0x" ++ toHex16 (to model.machine.cpu.sp))]
        ]
      ],
      div [style "display" "flex"
          , style "justify-content" "center"
          , style "align-items" "top"
          ] [
        div [id "memory", class "flex-fill"] [
          table [class "table table-sm"]
            (List.map
              (\i ->
                case Memory.get (u16from (i * 2)) model.machine.memory of
                  Just v -> tr [] [ td [] [Html.text (toHex16 (i * 2))], td [] [Html.text (Hex.toString (bits v))]]
                  _ -> tr [] [])
              (List.range 0 100))
        ],
        div [id "stack", class "flex-fill"] [
          table [class "table table-sm"]
            (List.map
              (\i ->
                case Memory.get (u16from (0xFDF0 + i * 2)) model.machine.memory of
                  Just v -> tr [] [ td [] [Html.text (toHex16 (0xFDF0 + i * 2))], td [] [Html.text (Hex.toString (bits v))]]
                  _ -> tr [] [])
              (List.range 0 100))
        ]
      ]
    ]
  ]

screen : Model -> Html Msg
screen model =
  div
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    ]
    [ lazy (\_ -> model.screen) model.screen ]

render : Model -> Html Msg
render model = 
  let
    data = shapes [ fill (Graphics.getColor model.machine.graphics.bg model.machine.graphics.palette) ] [ rect (0, 0) Graphics.width Graphics.height ] :: Graphics.produce model.machine.graphics
  in
    Canvas.toHtml (round Graphics.width, round Graphics.height) []
      (clear (0, 0) Graphics.width Graphics.height :: data)

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [Html.text "CHIP16"]
    , div [class "d-flex flex-row"] [
        div [id "screen"] [
          screen model
          , regs_table model
        ],
        inspector model
    ]
    , controls model
    ]

setKey : Keyboard.Key -> Number I16 -> Number I16
setKey key acc =
  let
    mask i = Numbers.shl (i16from 1) (i16from i)
    set ki = Numbers.or acc (Chip16.keyMap ki |> mask)
  in
  case key of
    Keyboard.ArrowUp -> set Chip16.Up
    Keyboard.ArrowDown -> set Chip16.Down
    Keyboard.ArrowLeft -> set Chip16.Left
    Keyboard.ArrowRight -> set Chip16.Right
    Keyboard.Character "A" -> set Chip16.A
    Keyboard.Character "B" -> set Chip16.B
    Keyboard.Shift -> set Chip16.Select
    Keyboard.Enter -> set Chip16.Start
    _ -> i16from 0

take_step : Model -> Bool -> Model
take_step model vblank =
  let
    controller = if vblank then
      Just (List.foldl
        setKey
        (i16from 0)
        model.pressedKeys)
      else Nothing
    machine = case controller of
      Just c -> Chip16.setController c model.machine
      Nothing -> model.machine
  in
    case prefetch model of
      Instruction a b c d ->
        { model
        | machine = Chip16.dispatch machine vblank (i8from a) (i8from b) (i8from c) (i8from d)
        , tick = if vblank then 0 else model.tick + 1 }

steps : Int -> Model -> Bool -> Model
steps n model vblank =
  List.foldl
    (\i the_model -> if i == 1 then take_step the_model vblank else take_step the_model False)
    model
    (List.range 1 n)

loadRom : Bytes -> Model -> (Model, Cmd Msg)
loadRom b model =
  let
    hdr = Decode.decode headerDecoder b
    rom = case hdr of
      Just h -> Decode.decode (romDecoder h) b
      Nothing -> Nothing
  in (
    { model
    | file = Just b
    , machine = case (hdr, rom) of
        (Just theHdr, Just theRom) -> case Decode.decode (bytesToMemory theHdr.romsz) theRom of
          Just memory -> Chip16.initFrom memory
          _ -> Debug.todo "failed to parse bytes to memory!"
        _ -> Debug.todo "failed to parse hdr/rom!"
    , tick = 0 }
    , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FileRequested -> (model, Select.file ["application/octet-stream"] FileLoaded)
    FileLoaded f -> (model, Task.perform FileContentLoaded (File.toBytes f))
    FileContentLoaded b -> loadRom b model
    Step n force vblank -> if model.running || force then (steps n model vblank, Cmd.none) else (model, Cmd.none)
    Running b -> ({ model | running = b }, Cmd.none)
    Reset -> ({ model | machine = Chip16.init, tick = 0 }, Cmd.none)
    KeyMsg keyMsg -> ({ model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none)
    Render -> ({ model | screen = render model }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 10 (\_ -> Step 10000 False False)
    , Time.every 16 (\_ -> Step 1 False True)
    , Time.every 16 (\_ -> Render)
    , Sub.map KeyMsg Keyboard.subscriptions
    ]