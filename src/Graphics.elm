module Graphics exposing (Command(..), Graphics, Palette(..), produce, append)

import Canvas exposing (Shape, Renderable, rect, shapes)
import Canvas.Settings exposing (..)
import Color exposing (Color)
import Slice exposing (Slice)
import Bitwise exposing (and, shiftRightBy)

type Palette = Palette (Slice Int)
type alias Graphics =
  { palette : Palette
  , bg : Int
  , spritew : Int
  , spriteh : Int
  , hflip : Bool
  , vflip : Bool
  , cmdbuffer : List (Command) }
type Command = Command (Float, Float) Int

extractColor : Int -> Color
extractColor col =
    let
        r = and 0xFF (shiftRightBy 16 col)
        g = and 0xFF (shiftRightBy 8 col)
        b = and 0xFF col
    in
        Color.rgb255 r g b

append : List (Command) -> Graphics -> Graphics
append cmds g =
    { g | cmdbuffer = List.append g.cmdbuffer cmds }

-- resolve colors and produce a final list of renderables
-- TODO: could optimize by batching, but take it easy
produce : List (Command) -> Palette -> List (Renderable)
produce cmds (Palette pal) =
    let
        produce_cmd : Command -> Renderable
        produce_cmd (Command (x, y) col) =
            case Slice.get col pal of
                Just color -> shapes [ fill (extractColor color) ] [ rect (x * 2, y * 2) 2 2 ]
                _ -> shapes [ fill Color.red ] [ rect (x * 2, y * 2) 2 2 ]
    in
        List.map
            produce_cmd
            --(List.reverse cmds)
            cmds