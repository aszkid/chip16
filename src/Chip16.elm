module Chip16 exposing (Cpu)

import Numbers exposing (..)

type alias Byte = Int

type alias Cpu =
  { pc : UInt16,
    sp : UInt16,
    r0 : Int16,
    r1 : Int16,
    r2 : Int16,
    r3 : Int16,
    r4 : Int16,
    r5 : Int16,
    r6 : Int16,
    r7 : Int16,
    r8 : Int16,
    r9 : Int16,
    ra : Int16,
    rb : Int16,
    rd : Int16,
    re : Int16,
    rf : Int16,
    flags : UInt8
  }

type alias GraphicsReg = 
  { bg : Int8,
    spritew : UInt8,
    spriteh : UInt8,
    hflip : Bool,
    vflip : Bool
  }
