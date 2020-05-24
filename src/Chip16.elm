module Chip16 exposing (Chip16, init, dispatch)

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
    regs : Slice Int16,
    flags : Flags
  }

type alias Chip16 = 
  { cpu : Cpu,
    memory : Memory }

initFlags : Flags
initFlags
  = { carry = False
      , zero = False
      , overflow = False
      , negative = False}

initCpu : Cpu
initCpu = 
  { pc = u16from 0
  , sp = u16from 0
  , regs = Slice.new 16 (i16from 0)
  , flags = initFlags}

set_rx : Cpu -> Int8 -> Int16 -> Cpu
set_rx cpu rx val
  = { cpu | regs = Slice.set (to (I8 rx)) val cpu.regs }

get_rx : Cpu -> Int8 -> Maybe Int16
get_rx cpu rx = Slice.get (to (I8 rx)) cpu.regs

set_sp : Cpu -> UInt16 -> Cpu
set_sp cpu val
  = { cpu | sp = val }

type FlagEnum = FCarry | FZero | FOverflow | FNegative
set_flag : FlagEnum -> Bool -> Flags -> Flags
set_flag flag val flags =
  case flag of
    FCarry -> { flags | carry = val }
    FZero -> { flags | zero = val }
    FOverflow -> { flags | overflow = val }
    FNegative -> { flags | negative = val }

set_flags : List (FlagEnum, Bool) -> Flags -> Flags
set_flags list flags =
  List.foldl (\(f, v) fs -> set_flag f v fs) flags list

{-- given two numbers and a CPU, add them
    and return the result and a new CPU with updated flags --}
add : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
add x y cpu =
  let
    (res, carry) = case Numbers.add (I16 x) (I16 y) of
      (I16 r, c) -> (r, c)
      _ -> Debug.todo "add failure"
    zero = isZero (I16 res)
    overflow = isPos (I16 res) && isNeg (I16 x) && isNeg (I16 y)
    negative = isNeg (I16 res)
    flags = [(FCarry, carry), (FZero, zero), (FOverflow, overflow), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

init : Chip16
init = 
  { cpu = initCpu,
    memory = Memory.init }

opLoad_RegImm : Chip16 -> Int8 -> Int16 -> Chip16
opLoad_RegImm machine rx val
  = { machine | cpu = set_rx machine.cpu rx val }

opLoad_SpImm : Chip16 -> UInt16 -> Chip16
opLoad_SpImm machine val
  = { machine | cpu = set_sp machine.cpu val }

opLoad_RegMem : Chip16 -> Int8 -> UInt16 -> Chip16
opLoad_RegMem machine rx addr =
  case Memory.get addr machine.memory of
    Just val -> { machine | cpu = set_rx machine.cpu rx val }
    _ -> Debug.todo ("invalid memory address: " ++ (Debug.toString addr))

opLoad_RegReg : Chip16 -> Int8 -> Int8 -> Chip16
opLoad_RegReg machine rx ry = Debug.todo "foobar!"

opMov : Chip16 -> Int8 -> Int8 -> Chip16
opMov machine rx ry =
  case get_rx machine.cpu ry of
    Just val -> { machine | cpu = set_rx machine.cpu rx val }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString ry))

opStore_Imm : Chip16 -> Int8 -> UInt16 -> Chip16
opStore_Imm machine rx addr =
  case get_rx machine.cpu rx of
    Just val -> { machine | memory = Memory.set addr val machine.memory }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString rx))

opStore_Reg : Chip16 -> Int8 -> Int8 -> Chip16
opStore_Reg machine rx ry =
  case get_rx machine.cpu ry of
    Just addr -> { machine | memory = Memory.set (tou16 (I16 addr)) (toi16 (I8 rx)) machine.memory }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString ry))

opAddi : Chip16 -> Int8 -> Int16 -> Chip16
opAddi machine rx val =
  case get_rx machine.cpu rx of
    Just vrx ->
      case add vrx val machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ ->  Debug.todo ("invalid register: " ++ (Debug.toString rx))

opAdd2 : Chip16 -> Int8 -> Int8 -> Chip16
opAdd2 machine rx ry =
  case (get_rx machine.cpu rx, get_rx machine.cpu ry) of
    (Just vx, Just vy) ->
      case add vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ ->  Debug.todo "invalid registers"

opAdd3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opAdd3 machine rx ry rz =
  case (get_rx machine.cpu rx, get_rx machine.cpu ry, get_rx machine.cpu rz) of
    (Just vx, Just vy, Just vz) ->
      case add vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ ->  Debug.todo "invalid registers"


opSubi machine rx hhll = machine
opSub2 machine rx ry = machine
opSub3 machine rx ry rz = machine
opCmpi machine rx hhll = machine
opCmp machine rx ry = machine

opAndi machine rx hhll = machine
opAnd2 machine rx ry = machine
opAnd3 machine rx ry rz = machine
opTsti machine rx hhll = machine
opTst machine rx ry = machine

opOri machine rx hhll = machine
opOr2 machine rx ry = machine
opOr3 machine rx ry rz = machine

opXori machine rx hhll = machine
opXor2 machine rx ry = machine
opXor3 machine rx ry rz = machine

opMuli machine rx hhll = machine
opMul2 machine rx ry = machine
opMul3 machine rx ry rz = machine

opDivi machine rx hhll = machine
opDiv2 machine rx ry = machine
opDiv3 machine rx ry rz = machine
opModi machine rx hhll = machine
opMod2 machine rx ry = machine
opMod3 machine rx ry rz = machine
opRemi machine rx hhll = machine
opRem2 machine rx ry = machine
opRem3 machine rx ry rz = machine

opShl machine rx n = machine
opShr machine rx n = machine
opSar machine rx n = machine
opSar2 machine rx ry = machine

opPush machine rx = machine
opPop machine rx = machine
opPushAll machine = machine
opPopAll machine = machine
opPushf machine = machine
opPopf machine = machine

opPalAddr machine hhll = machine
opPalReg machine rx = machine

opNoti machine rx hhll = machine
opNot1 machine rx = machine
opNot2 machine rx ry = machine
opNegi machine rx hhll = machine
opNeg1 machine rx = machine
opNeg2 machine rx ry = machine


opCls machine = machine
opVblnk machine = machine
opBgc machine n = machine
opSpr machine ll hh = machine
opDrwMem machine rx ry hhll = machine
opDrwReg machine rx ry rz = machine
opRnd machine rx hhll = machine
opFlip machine hflip vflip = machine
opSnd0 machine = machine
opSnd machine freq hhll = machine
opSnp machine rx hhll = machine
opSng machine ad vtsr = machine

opJmpi machine hhll = machine
opJmc machine hhll = machine
opJx machine x hhll = machine
opJme machine rx ry hhll = machine
opCalli machine hhll = machine
opRet machine = machine
opJmp machine rx = machine
opCx machine x hhll = machine
opCall machine rx = machine

-- attempt to dispatch an instruction from 4 bytes
dispatch : Chip16 -> Int8 -> Int8 -> Int8 -> Int8 -> Chip16
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
    case to (I8 a) of
      -- 0x Misc/Video/Audio
      0x00 -> machine
      0x01 -> opCls machine
      0x02 -> opVblnk machine
      0x03 -> opBgc machine n
      0x04 -> opSpr machine ll hh
      0x05 -> opDrwMem machine rx ry hhll
      0x06 -> opDrwReg machine rx ry rz
      0x07 -> opRnd machine rx hhll
      0x08 ->
        if isZero (I8 b) && isZero (I8 c) then
          case to (I8 d) of
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
      0x20 -> opLoad_RegImm machine rx (toi16 (U16 hhll))
      0x21 -> opLoad_SpImm machine hhll
      0x22 -> opLoad_RegMem machine b hhll
      0x23 -> opLoad_RegReg machine rx ry
      0x24 -> opMov machine rx ry
      -- 3x Stores
      0x30 -> opStore_Imm machine rx hhll
      0x31 -> opStore_Reg machine rx ry
      -- 4x Addition
      0x40 -> opAddi machine rx (toi16 (U16 hhll))
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