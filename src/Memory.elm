module Memory exposing (Memory, init, get, set)

import Numbers exposing (Int16, UInt16)
import Slice exposing (Slice)

type Memory = Memory (Slice Int16)

init = Memory (Slice.new 4096 0)

get : UInt16 -> Memory -> Maybe UInt16
get addr (Memory slice) = Slice.get addr slice

set : UInt16 -> UInt16 -> Memory -> Memory
set addr val (Memory slice) = Memory (Slice.set addr val slice)