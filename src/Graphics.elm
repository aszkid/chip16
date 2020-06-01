module Graphics exposing (Command(..), Graphics, Palette(..), putPixel, getColor, clear, produce, initGraphics)

import Canvas exposing (Renderable, rect, shapes)
import Canvas.Settings exposing (fill)
import Color exposing (Color)
import Bitwise exposing (and, shiftRightBy)
import Array exposing (Array)

type Palette = Palette (Array Int)
type alias Graphics =
  { palette : Palette
  , bg : Int
  , spritew : Int
  , spriteh : Int
  , hflip : Bool
  , vflip : Bool
  , cmds : List Command }
type Command = Command (Float, Float) Int

initPalette : Palette
initPalette = Palette
  ( Array.fromList
    [ 0x000000, 0x000000
    , 0x888888, 0xBF3932
    , 0xDE7AAE, 0x4C3D21
    , 0x905F25, 0xE49452
    , 0xEAD979, 0x537A3B
    , 0xABD54A, 0x252E38
    , 0x00467F, 0x68ABCC
    , 0xBCDEE4, 0xFFFFFF ])

initGraphics : Graphics
initGraphics =
  { palette = initPalette
  , bg = 0
  , spritew = 0
  , spriteh = 0
  , hflip = False
  , vflip = False
  , cmds = [] }

putPixel : (Float, Float) -> Int -> Graphics -> Graphics
putPixel (x, y) c g =
  { g | cmds = Command (x, y) c :: g.cmds }

extractColor : Int -> Color
extractColor col =
    let
        r = and 0xFF (shiftRightBy 16 col)
        g = and 0xFF (shiftRightBy 8 col)
        b = and 0xFF col
    in
        Color.rgb (toFloat r / 255) (toFloat g / 255) (toFloat b / 255)

getColor : Int -> Palette -> Color
getColor i (Palette pal) =
  case Array.get i pal of
    Just color -> extractColor color
    _ -> Color.red

-- resolve colors and produce a final list of renderables
produce : Graphics -> List (Renderable)
produce gfx =
    let
        pal = case gfx.palette of (Palette p) -> p
        produce_cmd : Command -> Renderable
        produce_cmd (Command (x, y) col) =
            case Array.get col pal of
                Just color -> shapes [ fill (extractColor color) ] [ rect (x * 2, y * 2) 2 2 ]
                _ -> shapes [ fill Color.white ] [ rect (x * 2, y * 2) 2 2 ]
    in
        List.map
            produce_cmd
            (List.reverse gfx.cmds)

clear : Graphics -> Graphics
clear g =
  { g | cmds = [] }