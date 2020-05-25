module Chip16 exposing (Chip16, init, dispatch, binFlags, initFlags)

import Numbers exposing (..)
import Slice exposing (Slice)
import Memory exposing (Memory)
import Bitwise exposing (or, shiftLeftBy)

type alias Flags =
  { carry : Bool
  , zero : Bool
  , overflow : Bool
  , negative : Bool }

binFlags : Flags -> Int16
binFlags fs = 
  let
    set bit yes to = Bitwise.or to (if yes then Bitwise.shiftLeftBy bit 1 else 0)
  in
    i16from (set 1 fs.carry (set 2 fs.zero (set 6 fs.overflow (set 7 fs.negative 0))))

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

get_rx2 : Cpu -> Int8 -> Int8 -> Maybe (Int16, Int16)
get_rx2 cpu rx ry =
  case (get_rx cpu rx, get_rx cpu ry) of
    (Just vx, Just vy) -> Just (vx, vy)
    _ -> Nothing

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

sub : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
sub x y cpu =
  let
    (res, borrow) = case Numbers.sub (I16 x) (I16 y) of
      (I16 r, b) -> (r, b)
      _ -> Debug.todo "sub failure"
    zero = isZero (I16 res)
    overflow = (isPos (I16 res) && isNeg (I16 x) && isPos (I16 y))
      || (isNeg (I16 res) && isPos (I16 x) && isNeg (I16 y))
    negative = isNeg (I16 res)
    flags = [(FCarry, borrow), (FZero, zero), (FOverflow, overflow), (FNegative, negative)]
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


opSubi : Chip16 -> Int8 -> Int16 -> Chip16
opSubi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case sub vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register rx"

opSub2 : Chip16 -> Int8 -> Int8 -> Chip16
opSub2 machine rx ry =
  case (get_rx machine.cpu rx, get_rx machine.cpu ry) of
    (Just vx, Just vy) ->
      case sub vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opSub3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opSub3 machine rx ry rz =
  case (get_rx machine.cpu rx, get_rx machine.cpu ry) of
    (Just vx, Just vy) ->
      case sub vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid regsiters"

opCmpi : Chip16 -> Int8 -> Int16 -> Chip16
opCmpi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case sub vx hhll machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

opCmp : Chip16 -> Int8 -> Int8 -> Chip16
opCmp machine rx ry =
  case (get_rx machine.cpu rx, get_rx machine.cpu ry) of
    (Just vx, Just vy) ->
      case sub vx vy machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid registers"

and : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
and x y cpu =
  let
    res = case Numbers.and (I16 x) (I16 y) of
      (I16 r) -> r
      _ -> Debug.todo "and failed"
    flags = [(FZero, isZero (I16 res)), (FNegative, isNeg (I16 res))]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opAndi : Chip16 -> Int8 -> Int16 -> Chip16
opAndi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case and vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opAnd2 : Chip16 -> Int8 -> Int8 -> Chip16
opAnd2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opAnd3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opAnd3 machine rx ry rz =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid registers"

opTsti : Chip16 -> Int8 -> Int16 -> Chip16
opTsti machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case and vx hhll machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

opTst : Chip16 -> Int8 -> Int8 -> Chip16
opTst machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

or : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
or x y cpu =
  let
    res = case Numbers.or (I16 x) (I16 y) of
      (I16 r) -> r
      _ -> Debug.todo "or failed"
    flags = [(FZero, isZero (I16 res)), (FNegative, isNeg (I16 res))]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opOri : Chip16 -> Int8 -> Int16 -> Chip16
opOri machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case or vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opOr2 : Chip16 -> Int8 -> Int8 -> Chip16
opOr2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case or vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opOr3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opOr3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case or vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

xor : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
xor x y cpu =
  let
    res = case Numbers.xor (I16 x) (I16 y) of
      (I16 r) -> r
      _ -> Debug.todo "xor failed"
    flags = [(FZero, isZero (I16 res)), (FNegative, isNeg (I16 res))]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opXori : Chip16 -> Int8 -> Int16 -> Chip16
opXori machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case xor vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opXor2 : Chip16 -> Int8 -> Int8 -> Chip16
opXor2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case xor vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opXor3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opXor3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case xor vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"


{-- given two numbers and a CPU, multiply them
    and return the result and a new CPU with updated flags --}
mul : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
mul x y cpu =
  let
    (res, carry) = case Numbers.mul (I16 x) (I16 y) of
      (I16 r, c) -> (r, c)
      _ -> Debug.todo "mul failure"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FCarry, carry), (FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opMuli : Chip16 -> Int8 -> Int16 -> Chip16
opMuli machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case mul vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMul2 : Chip16 -> Int8 -> Int8 -> Chip16
opMul2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mul vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMul3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opMul3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mul vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, divide them
    and return the result and a new CPU with updated flags --}
div : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
div x y cpu =
  let
    (res, remainder) = case Numbers.div (I16 x) (I16 y) of
      (I16 r, rr) -> (r, rr)
      _ -> Debug.todo "div failure"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FCarry, remainder), (FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opDivi : Chip16 -> Int8 -> Int16 -> Chip16
opDivi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case div vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opDiv2 : Chip16 -> Int8 -> Int8 -> Chip16
opDiv2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case div vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opDiv3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opDiv3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case div vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, take the first modulo the second
    returning the result and a new CPU with updated flags --}
mod : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
mod x y cpu =
  let
    res = case Numbers.mod (I16 x) (I16 y) of
      (I16 r) -> r
      _ -> Debug.todo "mod failed"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opModi : Chip16 -> Int8 -> Int16 -> Chip16
opModi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case mod vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMod2 : Chip16 -> Int8 -> Int8 -> Chip16
opMod2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mod vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMod3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opMod3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mod vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, take the remainder of the first
    divided by the second, returning the result and a new CPU with updated flags --}
rem : Int16 -> Int16 -> Cpu -> (Int16, Cpu)
rem x y cpu =
  let
    res = case Numbers.rem (I16 x) (I16 y) of
      (I16 r) -> r
      _ -> Debug.todo "mod failed"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opRemi : Chip16 -> Int8 -> Int16 -> Chip16
opRemi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case rem vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opRem2 : Chip16 -> Int8 -> Int8 -> Chip16
opRem2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case rem vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opRem3 : Chip16 -> Int8 -> Int8 -> Int8 -> Chip16
opRem3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case rem vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

type Dir = ShiftLeft | ShiftRight
shift : Int16 -> Int16 -> Dir -> Numbers.Shift -> Cpu -> (Int16, Cpu)
shift num by dir t cpu =
  let
    res = case (case dir of
      ShiftLeft -> Numbers.shl (I16 num) (I16 by)
      ShiftRight -> Numbers.shr (I16 num) (I16 by) t) of
        (I16 r) -> r
        _ -> Debug.todo "shift failed!"
    
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opShli : Chip16 -> Int8 -> Int16 -> Chip16
opShli machine rx n =
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftLeft ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opShri : Chip16 -> Int8 -> Int16 -> Chip16
opShri machine rx n = 
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftRight ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opSari : Chip16 -> Int8 -> Int16 -> Chip16
opSari machine rx n = 
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftRight ShiftArithmetic machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opShl2 : Chip16 -> Int8 -> Int8 -> Chip16
opShl2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftLeft ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opShr2 : Chip16 -> Int8 -> Int8 -> Chip16
opShr2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftRight ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opSar2 : Chip16 -> Int8 -> Int8 -> Chip16
opSar2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftRight ShiftArithmetic machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opPush : Chip16 -> Int8 -> Chip16
opPush machine rx =
  let
    new_sp = case Numbers.add (U16 (u16from 2)) (U16 machine.cpu.sp) of
      (U16 res, _) -> res
      _ -> Debug.todo "add failed"
  in
    case get_rx machine.cpu rx of
      Just vx -> { machine
                | memory = Memory.set machine.cpu.sp vx machine.memory
                , cpu = set_sp machine.cpu new_sp }
      _ -> Debug.todo "invalid register"

opPop : Chip16 -> Int8 -> Chip16
opPop machine rx = 
  let
    new_sp = case Numbers.sub (I16 (toi16 (U16 machine.cpu.sp))) (I16 (i16from 2)) of
      (I16 res, _) -> (tou16 (I16 res))
      _ -> Debug.todo "add failed"
    cpu = set_sp machine.cpu new_sp
    val = case Memory.get new_sp machine.memory of
      Just v -> v
      _ -> Debug.todo "invalid address at sp"
  in
    { machine
    | cpu = set_rx cpu rx val }

opPushAll : Chip16 -> Chip16
opPushAll machine = 
  let
    new_sp = case Numbers.add (U16 (u16from 32)) (U16 machine.cpu.sp) of
      (U16 res, _) -> res
      _ -> Debug.todo "add failed"
    adder x = case Numbers.add (U16 machine.cpu.sp) (U16 (u16from (x * 2))) of
      (U16 res, _) -> res
      _ -> Debug.todo "adder failed"
    
    setter : Int -> Memory -> Memory
    setter rx mem =
      case get_rx machine.cpu (i8from rx) of
        Just vx -> Memory.set (adder rx) vx mem
        _ -> Debug.todo "reg get failed"
    theMemory =
      List.foldl
        setter
        machine.memory
        (List.range 0 15)
  in
    { machine
    | memory = theMemory
    , cpu = set_sp machine.cpu new_sp }


opPopAll : Chip16 -> Chip16
opPopAll machine = 
  let
    new_sp = case Numbers.sub (I16 (toi16 (U16 machine.cpu.sp))) (I16 (i16from 32)) of
      (I16 res, _) -> (tou16 (I16 res))
      _ -> Debug.todo "add failed"
    cpu = set_sp machine.cpu new_sp
    adder x = case Numbers.add (U16 cpu.sp) (U16 (u16from (x * 2))) of
      (U16 res, _) -> res
      _ -> Debug.todo "adder failed"
    
    setter : Int -> Cpu -> Cpu
    setter rx cpu_ =
      case Memory.get (adder rx) machine.memory of
        Just vx -> set_rx cpu_ (i8from rx) vx
        _ -> Debug.todo "mem get failed"
    theCpu =
      List.foldl
        setter
        cpu
        (List.range 0 15)
  in
    { machine
    | cpu = theCpu }

opPushf : Chip16 -> Chip16
opPushf machine =
  let
    new_sp = case Numbers.add (U16 (u16from 2)) (U16 machine.cpu.sp) of
      (U16 res, _) -> res
      _ -> Debug.todo "add failed"
    flags = binFlags machine.cpu.flags
  in
    { machine 
    | memory = Memory.set machine.cpu.sp flags machine.memory
    , cpu = set_sp machine.cpu new_sp }

opPopf : Chip16 -> Chip16
opPopf machine = 
  let
    new_sp = case Numbers.sub (I16 (toi16 (U16 machine.cpu.sp))) (I16 (i16from 2)) of
      (I16 res, _) -> (tou16 (I16 res))
      _ -> Debug.todo "add failed"
    cpu = set_sp machine.cpu new_sp
    flags = binFlags machine.cpu.flags
  in
    { machine
    | cpu = cpu
    , memory = Memory.set cpu.sp flags machine.memory }

opPalAddr machine hhll = machine
opPalReg machine rx = machine

not : Int16 -> Cpu -> (Int16, Cpu)
not v cpu = 
  let
    res = case Numbers.not (I16 v) of
      I16 r -> r
      _ -> Debug.todo "failed to not!"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

neg : Int16 -> Cpu -> (Int16, Cpu)
neg v cpu = 
  let
    res = case Numbers.neg (I16 v) of
      I16 r -> r
      _ -> Debug.todo "failed to neg!"
    zero = isZero (I16 res)
    negative = isNeg (I16 res)
    flags = [(FZero, zero), (FNegative, negative)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opNoti : Chip16 -> Int8 -> Int16 -> Chip16
opNoti machine rx hhll =
  case not hhll machine.cpu of
    (res, cpu) -> { machine
                  | cpu = set_rx cpu rx res }

opNot1 : Chip16 -> Int8 -> Chip16
opNot1 machine rx =
  case get_rx machine.cpu rx of
    Just vx ->
      case not vx machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNot2 : Chip16 -> Int8 -> Int8 -> Chip16
opNot2 machine rx ry =
  case get_rx machine.cpu ry of
    Just vy ->
      case not vy machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNegi : Chip16 -> Int8 -> Int16 -> Chip16
opNegi machine rx hhll = 
  case neg hhll machine.cpu of
    (res, cpu) -> { machine
                  | cpu = set_rx cpu rx res }

opNeg1 : Chip16 -> Int8 -> Chip16
opNeg1 machine rx = 
  case get_rx machine.cpu rx of
    Just vx ->
      case neg vx machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNeg2 : Chip16 -> Int8 -> Int8 -> Chip16
opNeg2 machine rx ry = 
  case get_rx machine.cpu ry of
    Just vy ->
      case neg vy machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"


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
    n = toi16 (I8 (nibbleLO c))
    hhll = buildLE c d
    hh = d
    ll = c
    vtsr = buildLE c d
    ad = b
    x = nibbleLO b
  in
    case tobits (I8 a) of
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
      0x50 -> opSubi machine rx (toi16 (U16 hhll))
      0x51 -> opSub2 machine rx ry
      0x52 -> opSub3 machine rx ry rz
      0x53 -> opCmpi machine rx (toi16 (U16 hhll))
      0x54 -> opCmp machine rx ry
      -- 6x Bitwise AND
      0x60 -> opAndi machine rx (toi16 (U16 hhll))
      0x61 -> opAnd2 machine rx ry
      0x62 -> opAnd3 machine rx ry rz
      0x63 -> opTsti machine rx (toi16 (U16 hhll))
      0x64 -> opTst machine rx ry
      -- 7x Bitwise OR
      0x70 -> opOri machine rx (toi16 (U16 hhll))
      0x71 -> opOr2 machine rx ry
      0x72 -> opOr3 machine rx ry rz
      -- 8x Bitwise XOR
      0x80 -> opXori machine rx (toi16 (U16 hhll))
      0x81 -> opXor2 machine rx ry
      0x82 -> opXor3 machine rx ry rz
      -- 9x Multiplication
      0x90 -> opMuli machine rx (toi16 (U16 hhll))
      0x91 -> opMul2 machine rx ry
      0x92 -> opMul3 machine rx ry rz
      -- Ax Division
      0xA0 -> opDivi machine rx (toi16 (U16 hhll))
      0xA1 -> opDiv2 machine rx ry
      0xA2 -> opDiv3 machine rx ry rz
      0xA3 -> opModi machine rx (toi16 (U16 hhll))
      0xA4 -> opMod2 machine rx ry
      0xA5 -> opMod3 machine rx ry rz
      0xA6 -> opRemi machine rx (toi16 (U16 hhll))
      0xA7 -> opRem2 machine rx ry
      0xA8 -> opRem3 machine rx ry rz
      -- Bx Logical/Arithmetic Shifts
      0xB0 -> opShli machine rx n
      0xB1 -> opShri machine rx n
      0xB2 -> opSari machine rx n
      0xB3 -> opShl2 machine rx ry
      0xB4 -> opShr2 machine rx ry
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
      0xE0 -> opNoti machine rx (toi16 (U16 hhll))
      0xE1 -> opNot1 machine rx
      0xE2 -> opNot2 machine rx ry
      0xE3 -> opNegi machine rx (toi16 (U16 hhll))
      0xE4 -> opNeg1 machine rx
      0xE5 -> opNeg2 machine rx ry
      _ -> Debug.todo ("instruction `" ++  Debug.toString (to (I8 a)) ++ " ` does not exist!")

{--

-- decodes an instruction from a byte stream
decode : Decoder (Maybe Instruction)

-- executes an instruction, returning an updated machine state
exec : Chip16 -> Instruction -> Chip16

--}