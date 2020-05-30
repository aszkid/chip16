module Memory exposing (Memory, init, get, get8, set, set8, stackAddr)

import Numbers exposing (Int8, UInt16, Int16, i8from, u16from, to, ChipInt (..))
import Slice exposing (Slice)

type Memory = Memory (Slice Int8)
memoryLength = 65536
stackAddr : UInt16
stackAddr = u16from 0xFDF0

init : Memory
init = Memory (Slice.new memoryLength (i8from 0))

get8 : UInt16 -> Memory -> Maybe Int8
get8 addr (Memory slice) = Slice.get (to (U16 addr)) slice

get : UInt16 -> Memory -> Maybe Int16
get addr memory = 
  let
    addrplus1 = case Numbers.add__ (U16 addr) (U16 (u16from 1)) of
        U16 v -> v
        _ -> Debug.todo "can't add numbers lol"
  in
    case (get8 addr memory, get8 addrplus1 memory) of
      (Just ll, Just hh) -> Just (Numbers.buildLEs ll hh)
      _ -> Debug.todo "invalid address!"

set8 : UInt16 -> Int8 -> Memory -> Memory
set8 addr val (Memory slice) = Memory (Slice.set (to (U16 addr)) val slice)

set : UInt16 -> Int16 -> Memory -> Memory
set addr val memory =
  let
    addrplus1 = case Numbers.add__ (U16 addr) (U16 (u16from 1)) of
        U16 v -> v
        _ -> Debug.todo "invalid address!"
  in
    case Numbers.unpackLE val of
        (ll, hh) -> set8 addr ll (set8 addrplus1 hh memory)