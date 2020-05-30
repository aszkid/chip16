module Memory exposing (Memory, init, initFrom, get, set)

import Numbers exposing (Int16, UInt16, i16from, to, ChipInt (..))
import Slice exposing (Slice)

type Memory = Memory (Slice Int16)
memoryLength = 65536

init : Memory
init = Memory (Slice.new memoryLength (i16from 0))

initFrom : Slice Int16 -> Memory
initFrom rom =
    let
        romlen = Slice.length rom
        remaining = max 0 (memoryLength - romlen)
    in
        Memory (Slice.append rom (Slice.fromList (List.repeat remaining (i16from 0))))

get : UInt16 -> Memory -> Maybe Int16
get addr (Memory slice) = Slice.get (to (U16 addr) // 2) slice

set : UInt16 -> Int16 -> Memory -> Memory
set addr val (Memory slice) = Memory (Slice.set (to (U16 addr) // 2) val slice)