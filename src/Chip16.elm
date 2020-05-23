module Chip16 exposing (Chip16, init)

import Numbers exposing (..)
import Memory exposing (Memory)

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
    rc : Int16,
    rd : Int16,
    re : Int16,
    rf : Int16,
    flags : UInt8
  }

type alias Chip16 = 
  { cpu : Cpu,
    memory : Memory }

initCpu : Cpu
initCpu = 
  { pc = 0,
    sp = 0,
    r0 = 0,
    r1 = 0,
    r2 = 0,
    r3 = 0,
    r4 = 0,
    r5 = 0,
    r6 = 0,
    r7 = 0,
    r8 = 0,
    r9 = 0,
    ra = 0,
    rb = 0,
    rc = 0,
    rd = 0,
    re = 0,
    rf = 0,
    flags = 0 }

init : Chip16
init = 
  { cpu = initCpu,
    memory = Memory.init }