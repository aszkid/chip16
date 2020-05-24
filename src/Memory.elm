module Memory exposing (Memory, init, get, set)

import Numbers exposing (Int16, UInt16, i16from, to, ChipInt (..))
import Slice exposing (Slice)

type Memory = Memory (Slice Int16)

init : Memory
init = Memory (Slice.new 16 (i16from 0))

get : UInt16 -> Memory -> Maybe Int16
get addr (Memory slice) = Slice.get (to (U16 addr)) slice

set : UInt16 -> Int16 -> Memory -> Memory
set addr val (Memory slice) = Memory (Slice.set (to (U16 addr)) val slice)