module Graphics exposing (Command(..), Graphics, Palette(..), putPixel, getColor, clear, produce, initGraphics)

import Canvas exposing (Shape, Renderable, rect, shapes)
import Canvas.Settings exposing (..)
import Color exposing (Color)
import Slice exposing (Slice)
import Bitwise exposing (and, shiftRightBy)
import Array exposing (Array)

type Palette = Palette (Slice Int)
type alias Graphics =
  { palette : Palette
  , bg : Int
  , spritew : Int
  , spriteh : Int
  , hflip : Bool
  , vflip : Bool
  , screen : Array Int }
type Command = Command (Float, Float) Int

initPalette : Palette
initPalette = Palette
  ( Slice.fromList
    [ 0x000000, 0x000000
    , 0x888888, 0xBF3932
    , 0xDE7AAE, 0x4C3D21
    , 0x905F25, 0xE49452
    , 0xEAD979, 0x537A3B
    , 0xABD54A, 0x252E38
    , 0x00467F, 0x68ABCC
    , 0xBCDEE4, 0xFFFFFF ])

initScreen : Array Int
initScreen = Array.repeat 76800 0

initGraphics : Graphics
initGraphics =
  { palette = initPalette
  , bg = 0
  , spritew = 0
  , spriteh = 0
  , hflip = False
  , vflip = False
  , screen = initScreen }


putPixel : (Float, Float) -> Int -> Graphics -> Graphics
putPixel (x, y) c g =
  { g | screen = Array.set (round (x + y * 320)) c g.screen }

produce : Graphics -> Array (Int)
produce g =
  let
    pal = case g.palette of Palette p -> p
    produce_cmd : Int -> Int
    produce_cmd col =
      if col == 0 then g.bg else
        case Slice.get col pal of
            Just color -> color
            _ -> 0xfc0303 -- red
  in
    Array.map
      produce_cmd
      g.screen

extractColor : Int -> Color
extractColor col =
    let
        r = and 0xFF (shiftRightBy 16 col)
        g = and 0xFF (shiftRightBy 8 col)
        b = and 0xFF col
    in
        Color.rgb255 r g b

getColor : Int -> Palette -> Color
getColor i (Palette pal) =
  case Slice.get i pal of
    Just color -> extractColor color
    _ -> Color.red

{--append : List (Command) -> Graphics -> Graphics
append cmds g =
    { g | cmdbuffer = List.append g.cmdbuffer cmds }--}

-- resolve colors and produce a final list of renderables
-- TODO: could optimize by batching, but take it easy
{--produce : List (Command) -> Palette -> List (Renderable)
produce cmds (Palette pal) =
    let
        produce_cmd : Command -> Renderable
        produce_cmd (Command (x, y) col) =
            case Slice.get col pal of
                Just color -> shapes [ fill (extractColor color) ] [ rect (x * 2, y * 2) 2 2 ]
                _ -> shapes [ fill Color.white ] [ rect (x * 2, y * 2) 2 2 ]
    in
        List.map
            produce_cmd
            --(List.reverse cmds)
            cmds--}

clear : Graphics -> Graphics
clear g =
  { g | screen = initScreen }