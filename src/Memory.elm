module Memory exposing (Memory, init, get, get8, set, set8, stackAddr)

import Array exposing (Array)
import Numbers2 as Numbers exposing (..)

type Memory = Memory (Array (Number I8))

memoryLength : Int
memoryLength = 65536
stackAddr : Number U16
stackAddr = u16from 0xFDF0

init : Memory
init = Memory (Array.repeat memoryLength (i8from 0))

get8 : Number U16 -> Memory -> Maybe (Number I8)
get8 addr (Memory slice) = Array.get (to addr) slice

get : Number U16 -> Memory -> Maybe (Number I16)
get addr memory = 
  case (get8 addr memory, get8 (Numbers.add addr (u16from 1)) memory) of
    (Just ll, Just hh) -> Just (Numbers.i16build ll hh)
    _ -> Debug.todo "invalid address!"

set8 : Number U16 -> Number I8 -> Memory -> Memory
set8 addr val (Memory slice) = Memory (Array.set (to addr) val slice)

set : Number U16 -> Number I16 -> Memory -> Memory
set addr val memory =
  let
    addrplus1 = Numbers.add addr (u16from 1)
  in
    case Numbers.unpacki16 val of
        (ll, hh) -> set8 addr ll (set8 addrplus1 hh memory)