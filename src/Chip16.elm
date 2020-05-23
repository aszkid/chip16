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
    n = nibbleLO c
    hhll = buildLE c d
    hh = d
    ll = c
    vtsr = buildLE c d
    ad = b
    x = nibbleLO b
  in
    case a of
      -- 0x Misc/Video/Audio
      0x00 -> machine
      0x01 -> opCls machine
      0x02 -> opVblnk macine
      0x03 -> opBgc machine n
      0x04 -> opSpr machine ll hh
      0x05 -> opDrwMem machine rx ry hhll
      0x06 -> opDrwReg machine rx ry rz
      0x07 -> opRnd machine rx hhll
      0x08 ->
        if b == 0 && c == 0 then
          case d of
            0x00 -> opFlip machine False False
            0x01 -> opFlip machine False True
            0x02 -> opFlip machine True False
            0x03 -> opFlip machine True True
            _ -> machine
        else machine
      0x09 -> opSnd0 machine
      0x0A -> opSnd machine 500 hhll
      0x0B -> opSnd machine 1000 hhll
      0x0C -> opSnd machine 1500 hhll
      0x0D -> opSnp machine rx hhll
      0x0E -> opSng machine ad vtsr
      -- 1x Jumps
      0x10 -> opJmpi machine hhll
      0x11 -> opJmc machine hhll
      0x12 -> opJx machine x hhll
      0x13 -> opJme machine rx ry hhll
      0x14 -> opCalli machine hhll
      0x15 -> opRet machine
      0x16 -> opJmp machine rx
      0x17 -> opCx machine x hhll
      0x18 -> opCall machine rx
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
      0x50 -> opSubi machine rx hhll
      0x51 -> opSub2 machine rx ry
      0x52 -> opSub3 machine rx ry rz
      0x53 -> opCmpi machine rx hhll
      0x54 -> opCmp machine rx ry
      -- 6x Bitwise AND
      0x60 -> opAndi machine rx hhll
      0x61 -> opAnd2 machine rx ry
      0x62 -> opAnd3 machine rx ry rz
      0x63 -> opTsti machine rx hhll
      0x64 -> opTst machine rx ry
      -- 7x Bitwise OR
      0x70 -> opOri machine rx hhll
      0x71 -> opOr2 machine rx ry
      0x72 -> opOr3 machine rx ry rz
      -- 8x Bitwise XOR
      0x80 -> opXori machine rx hhll
      0x81 -> opXor2 machine rx ry
      0x82 -> opXor3 machine rx ry rz
      -- 9x Multiplication
      0x90 -> opMuli machine rx hhll
      0x91 -> opMul2 machine rx ry
      0x92 -> opMul3 machine rx ry rz
      -- Ax Division
      0xA0 -> opDivi machine rx hhll
      0xA1 -> opDiv2 machine rx ry
      0xA2 -> opDiv3 machine rx ry rz
      0xA3 -> opModi machine rx hhll
      0xA4 -> opMod2 machine rx ry
      0xA5 -> opMod3 machine rx ry rz
      0xA6 -> opRemi machine rx hhll
      0xA7 -> opRem2 machine rx ry
      0xA8 -> opRem3 machine rx ry rz
      -- Bx Logical/Arithmetic Shifts
      0xB0 -> opShl machine rx n
      0xB1 -> opShr machine rx n
      0xB2 -> opSar machine rx n
      0xB3 -> opShl machine rx ry
      0xB4 -> opShr machine rx ry
      0xB5 -> opSar2 machine rx ry
      -- Cx Push/Pop
      0xC0 -> opPush machine rx
      0xC1 -> opPop machine rx
      0xC2 -> opPushAll machine
      0xC3 -> opPopAll machine
      0xC4 -> opPushf machine
      0xC5 -> opPopf machine
      -- Dx Palette
      0xD0 -> opPalAddr machine hhll
      0xD1 -> opPalReg machine rx
      -- Ex Not/Neg
      0xE0 -> opNoti machine rx hhll
      0xE1 -> opNot1 machine rx
      0xE2 -> opNot2 machine rx ry
      0xE3 -> opNegi machine rx hhll
      0xE4 -> opNeg1 machine rx
      0xE5 -> opNeg2 machine rx ry
      _ -> machine

{--

-- decodes an instruction from a byte stream
decode : Decoder (Maybe Instruction)

-- executes an instruction, returning an updated machine state
exec : Chip16 -> Instruction -> Chip16

--}