module Screen exposing (Screen, init)

import Slice exposing (Slice)
import Numbers exposing (UInt8)

type alias Screen = Slice UInt8

width = 340
height = 240

init : Screen
init = Slice.new 76800 0

get : Int -> Int -> Screen -> Maybe UInt8
get x y scr = Slice.get (y * width + x) scr