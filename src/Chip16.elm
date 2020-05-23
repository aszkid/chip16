module Chip16 exposing (Chip16, init)

import Numbers exposing (..)
import Slice exposing (Slice)
import Memory exposing (Memory)

type alias Flags =
  { carry : Bool
  , zero : Bool
  , overflow : Bool
  , negative : Bool }

type alias Cpu =
  { pc : UInt16,
    sp : UInt16,
    regs : Slice UInt16,
    flags : Flags
  }

type alias Chip16 = 
  { cpu : Cpu,
    memory : Memory }

initCpu : Cpu
initCpu = 
  { pc = 0
  , sp = 0
  , regs = Slice.new 16 0
  , flags = 
      { carry = False
      , zero = False
      , overflow = False
      , negative = False}}

init : Chip16
init = 
  { cpu = initCpu,
    memory = Memory.init }

opLoad_RegImm : Chip16 -> Int -> UInt16 -> Chip16
opLoad_RegImm machine reg val = machine

opLoad_SpImm : Chip16 -> UInt16 -> Chip16
opLoad_SpImm machine val = machine

opLoad_RegMem : Chip16 -> Int -> UInt16 -> Chip16
opLoad_RegMem machine reg addr = machine

opLoad_RegReg : Chip16 -> Int -> Int -> Chip16
opLoad_RegReg machine rx ry = machine

opMov : Chip16 -> Int -> Int -> Chip16
opMov machine rx ry = machine

opStore_Imm : Chip16 -> Int8 -> UInt16 -> Chip16
opStore_Imm machine rx addr = machine

opStore_Reg : Chip16 -> Int8 -> UInt16 -> Chip16
opStore_Reg machine rx ry = machine

opAddi : Chip16 -> Int8 -> UInt16 -> Chip16
opAddi machine rx addr = machine

opAdd2 : Chip16 -> Int8 -> Int8 -> Chip16
opAdd2 machine rx ry = machine

opAdd3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opAdd3 machine rx ry rz = machine

-- attempt to dispatch an instruction from 4 bytes
dispatch : Chip16 -> UInt8 -> UInt8 -> UInt8 -> UInt8 -> Chip16
dispatch machine a b c d =
  let
    rx = nibbleLO b
    ry = nibbleHI b
    rz = nibbleLO c
    hhll = buildLE c d
  in
    case a of
      -- 2x Loads
      0x20 -> opLoad_RegImm machine rx hhll
      0x21 -> opLoad_SpImm machine hhll
      0x22 -> opLoad_RegMem machine b hhll
      0x23 -> opLoad_RegReg machine rx ry
      0x24 -> opMov machine rx ry
      -- 3x Stores
      0x30 -> opStore_Imm machine hhll b
      0x31 -> opStore_Reg machine rx ry
      -- 4x Addition
      0x40 -> opAddi machine rx hhll
      0x41 -> opAdd2 machine rx ry
      0x42 -> opAdd3 machine rx ry rz
      -- 5x Subtraction
      -- 6x Bitwise AND
      -- 7x Bitwise OR
      -- 8x Bitwise XOR
      -- 9x Multiplication
      -- Ax Division
      -- Bx Logical/Arithmetic Shifts
      -- Cx Push/Pop
      -- Dx Palette
      -- Ex Not/Neg
      _ -> machine

{--

-- decodes an instruction from a byte stream
decode : Decoder (Maybe Instruction)

-- executes an instruction, returning an updated machine state
exec : Chip16 -> Instruction -> Chip16

--}