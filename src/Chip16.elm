module Chip16 exposing (Chip16, init, initFrom, dispatch, keyMap, Key(..), setController, getController)

--import Numbers exposing (..)
import Numbers2 as Numbers exposing (..)
import Array exposing (Array)
import Memory exposing (Memory)
import Bitwise exposing (or, shiftLeftBy)
import Random
import Graphics exposing (..)

type alias Flags =
  { carry : Bool
  , zero : Bool
  , overflow : Bool
  , negative : Bool }

binFlags : Flags -> Number I16
binFlags fs = 
  let
    set bit yes to = Bitwise.or to (if yes then Bitwise.shiftLeftBy bit 1 else 0)
  in
    set 7 fs.negative 0 |> set 6 fs.overflow |> set 2 fs.zero |> set 1 fs.carry |> i16from

type alias Cpu =
  { pc : Number U16,
    sp : Number U16,
    regs : Array (Number I16),
    flags : Flags,
    seed : Random.Seed
  }    

type alias Chip16 = 
  { cpu : Cpu,
    memory : Memory,
    graphics : Graphics }

initFlags : Flags
initFlags
  = { carry = False
      , zero = False
      , overflow = False
      , negative = False}

initCpu : Cpu
initCpu = 
  { pc = u16from 0
  , sp = Memory.stackAddr
  , regs = Array.repeat 16 (i16from 0)
  , flags = initFlags
  , seed = Random.initialSeed 42 }

init : Chip16
init = 
  { cpu = initCpu
  , memory = Memory.init
  , graphics = initGraphics }

initFrom : Memory -> Chip16
initFrom memory = 
  { cpu = initCpu
  , memory = memory
  , graphics = initGraphics }

set_rx : Cpu -> Number I8 -> Number I16 -> Cpu
set_rx cpu rx val
  = { cpu | regs = Array.set (to rx) val cpu.regs }

get_rx : Cpu -> Number I8 -> Maybe (Number I16)
get_rx cpu rx = Array.get (to rx) cpu.regs

get_rx2 : Cpu -> Number I8 -> Number I8 -> Maybe (Number I16, Number I16)
get_rx2 cpu rx ry =
  case (get_rx cpu rx, get_rx cpu ry) of
    (Just vx, Just vy) -> Just (vx, vy)
    _ -> Nothing
    
get_rx3 : Cpu -> Number I8 -> Number I8 -> Number I8 -> Maybe (Number I16, Number I16, Number I16)
get_rx3 cpu rx ry rz =
  case (get_rx cpu rx, get_rx cpu ry, get_rx cpu rz) of
    (Just vx, Just vy, Just vz) -> Just (vx, vy, vz)
    _ -> Nothing

set_sp : Cpu -> Number U16 -> Cpu
set_sp cpu val
  = { cpu | sp = val }

set_pc : Cpu -> Number U16 -> Cpu
set_pc cpu val
  = { cpu | pc = val }

dec_pc : Int -> Cpu -> Cpu
dec_pc by cpu =
  { cpu | pc = Numbers.sub cpu.pc (u16from by) }

inc_pc : Int -> Cpu -> Cpu
inc_pc by cpu =
  { cpu | pc = Numbers.add cpu.pc (u16from by) }

set_seed : Random.Seed -> Cpu -> Cpu
set_seed seed cpu = { cpu | seed = seed }

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
  List.foldl (\(f, v) -> set_flag f v) flags list

set_palette : Palette -> Graphics -> Graphics
set_palette p g =
  { g | palette = p }

set_bg : Int -> Graphics -> Graphics
set_bg idx g =
  { g | bg = idx }

clear_fg : Graphics -> Graphics
clear_fg g = g

set_spritewh : Int -> Int -> Graphics -> Graphics
set_spritewh w h g =
  { g | spritew = w, spriteh = h }

set_flip : Bool -> Bool -> Graphics -> Graphics
set_flip hflip vflip g = 
  { g | hflip = hflip, vflip = vflip }

{-- given two numbers and a CPU, add them
    and return the result and a new CPU with updated flags --}
add : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
add x y cpu =
  let
    (res, carry) = Numbers.addC x y
    flags = [(FCarry, carry), (FZero, isZero res), (FOverflow, isPos res && isNeg x && isNeg y), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

sub : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
sub x y cpu =
  let
    (res, borrow) = Numbers.subC x y
    overflow = (isPos res && isNeg x && isPos y)
      || (isNeg res && isPos x && isNeg y)
    flags = [(FCarry, borrow), (FZero, isZero res), (FOverflow, overflow), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opLoad_RegImm : Chip16 -> Number I8 -> Number I16 -> Chip16
opLoad_RegImm machine rx val
  = { machine | cpu = set_rx machine.cpu rx val }

opLoad_SpImm : Chip16 -> Number U16 -> Chip16
opLoad_SpImm machine val
  = { machine | cpu = set_sp machine.cpu val }

opLoad_RegMem : Chip16 -> Number I8 -> Number U16 -> Chip16
opLoad_RegMem machine rx addr =
  case Memory.get addr machine.memory of
    Just val -> { machine | cpu = set_rx machine.cpu rx val }
    _ -> Debug.todo ("invalid memory address: " ++ (Debug.toString addr))

opLoad_RegReg : Chip16 -> Number I8 -> Number I8 -> Chip16
opLoad_RegReg machine rx ry =
  case get_rx machine.cpu ry of
    Just addr ->
      case Memory.get (intou16 addr) machine.memory of
        Just val -> { machine | cpu = set_rx machine.cpu rx val }
        _ -> Debug.todo "invalid memory address"
    _ -> Debug.todo "invalid register"

opMov : Chip16 -> Number I8 -> Number I8 -> Chip16
opMov machine rx ry =
  case get_rx machine.cpu ry of
    Just val -> { machine | cpu = set_rx machine.cpu rx val }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString ry))

opStore_Imm : Chip16 -> Number I8 -> Number U16 -> Chip16
opStore_Imm machine rx addr =
  case get_rx machine.cpu rx of
    Just val -> { machine | memory = Memory.set addr val machine.memory }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString rx))

opStore_Reg : Chip16 -> Number I8 -> Number I8 -> Chip16
opStore_Reg machine rx ry =
  case get_rx2 machine.cpu rx ry of
    Just (val, addr) -> { machine | memory = Memory.set (intou16 addr) val machine.memory }
    _ -> Debug.todo ("invalid register: " ++ (Debug.toString ry))

opAddi : Chip16 -> Number I8 -> Number I16 -> Chip16
opAddi machine rx val =
  case get_rx machine.cpu rx of
    Just vrx ->
      case add vrx val machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ ->  Debug.todo ("invalid register: " ++ (Debug.toString rx))

opAdd2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opAdd2 machine rx ry =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case add vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ ->  Debug.todo "invalid registers"

opAdd3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opAdd3 machine rx ry rz =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case add vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ ->  Debug.todo "invalid registers"


opSubi : Chip16 -> Number I8 -> Number I16 -> Chip16
opSubi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case sub vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register rx"

opSub2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opSub2 machine rx ry =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case sub vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opSub3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opSub3 machine rx ry rz =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case sub vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid regsiters"

opCmpi : Chip16 -> Number I8 -> Number I16 -> Chip16
opCmpi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case sub vx hhll machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

opCmp : Chip16 -> Number I8 -> Number I8 -> Chip16
opCmp machine rx ry =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case sub vx vy machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid registers"

and : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
and x y cpu =
  let
    res = Numbers.and x y
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opAndi : Chip16 -> Number I8 -> Number I16 -> Chip16
opAndi machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case and vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opAnd2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opAnd2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opAnd3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opAnd3 machine rx ry rz =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid registers"

opTsti : Chip16 -> Number I8 -> Number I16 -> Chip16
opTsti machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case and vx hhll machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

opTst : Chip16 -> Number I8 -> Number I8 -> Chip16
opTst machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case and vx vy machine.cpu of
        (_, cpu) -> { machine | cpu = cpu }
    _ -> Debug.todo "invalid register"

or : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
or x y cpu =
  let
    res = Numbers.or x y
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opOri : Chip16 -> Number I8 -> Number I16 -> Chip16
opOri machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case or vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opOr2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opOr2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case or vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opOr3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opOr3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case or vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

xor : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
xor x y cpu =
  let
    res = Numbers.xor x y
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opXori : Chip16 -> Number I8 -> Number I16 -> Chip16
opXori machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case xor vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opXor2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opXor2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case xor vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opXor3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opXor3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case xor vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"


{-- given two numbers and a CPU, multiply them
    and return the result and a new CPU with updated flags --}
mul : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
mul x y cpu =
  let
    (res, carry) = Numbers.mulC x y
    flags = [(FCarry, carry), (FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opMuli : Chip16 -> Number I8 -> Number I16 -> Chip16
opMuli machine rx hhll =
  case get_rx machine.cpu rx of
    Just vx ->
      case mul vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMul2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opMul2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mul vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMul3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opMul3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mul vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, divide them
    and return the result and a new CPU with updated flags --}
div : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
div x y cpu =
  let
    (res, remainder) = Numbers.divC x y
    flags = [(FCarry, remainder), (FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opDivi : Chip16 -> Number I8 -> Number I16 -> Chip16
opDivi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case div vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opDiv2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opDiv2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case div vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opDiv3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opDiv3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case div vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, take the first modulo the second
    returning the result and a new CPU with updated flags --}
mod : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
mod x y cpu =
  let
    res = Numbers.mod x y
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opModi : Chip16 -> Number I8 -> Number I16 -> Chip16
opModi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case mod vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMod2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opMod2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mod vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opMod3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opMod3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case mod vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

{-- given two numbers and a CPU, take the remainder of the first
    divided by the second, returning the result and a new CPU with updated flags --}
rem : Number I16 -> Number I16 -> Cpu -> (Number I16, Cpu)
rem x y cpu =
  let
    res = Numbers.rem x y
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opRemi : Chip16 -> Number I8 -> Number I16 -> Chip16
opRemi machine rx hhll = 
  case get_rx machine.cpu rx of
    Just vx ->
      case rem vx hhll machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opRem2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opRem2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case rem vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opRem3 : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opRem3 machine rx ry rz = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case rem vx vy machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rz res }
    _ -> Debug.todo "invalid register"

type Dir = ShiftLeft | ShiftRight
shift : Number I16 -> Number I16 -> Dir -> Numbers.Shift -> Cpu -> (Number I16, Cpu)
shift num by dir t cpu =
  let
    res =
      case dir of
        ShiftLeft -> Numbers.shl num by
        ShiftRight -> Numbers.shr num by t
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opShli : Chip16 -> Number I8 -> Number I16 -> Chip16
opShli machine rx n =
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftLeft ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opShri : Chip16 -> Number I8 -> Number I16 -> Chip16
opShri machine rx n = 
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftRight ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opSari : Chip16 -> Number I8 -> Number I16 -> Chip16
opSari machine rx n = 
  case get_rx machine.cpu rx of
    Just vx ->
      case shift vx n ShiftRight ShiftArithmetic machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opShl2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opShl2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftLeft ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opShr2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opShr2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftRight ShiftLogical machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opSar2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opSar2 machine rx ry = 
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      case shift vx vy ShiftRight ShiftArithmetic machine.cpu of
        (res, cpu) -> { machine | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid registers"

opPush : Chip16 -> Number I8 -> Chip16
opPush machine rx =
  let
    new_sp = Numbers.add (u16from 2) machine.cpu.sp
  in
    case get_rx machine.cpu rx of
      Just vx -> { machine
                | memory = Memory.set machine.cpu.sp vx machine.memory
                , cpu = set_sp machine.cpu new_sp }
      _ -> Debug.todo "invalid register"

opPop : Chip16 -> Number I8 -> Chip16
opPop machine rx = 
  let
    new_sp = Numbers.sub machine.cpu.sp (u16from 2)
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
    new_sp = Numbers.add (u16from 32) machine.cpu.sp
    adder x = Numbers.add machine.cpu.sp (u16from (x * 2))
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
    new_sp = Numbers.sub machine.cpu.sp (u16from 32)
    cpu = set_sp machine.cpu new_sp
    adder x = Numbers.add cpu.sp (u16from (x * 2))
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
    new_sp = Numbers.add (u16from 2) machine.cpu.sp
    flags = binFlags machine.cpu.flags
  in
    { machine 
    | memory = Memory.set machine.cpu.sp flags machine.memory
    , cpu = set_sp machine.cpu new_sp }

opPopf : Chip16 -> Chip16
opPopf machine = 
  let
    new_sp = Numbers.sub machine.cpu.sp (u16from 2)
    cpu = set_sp machine.cpu new_sp
    flags = binFlags machine.cpu.flags
  in
    { machine
    | cpu = cpu
    , memory = Memory.set cpu.sp flags machine.memory }

loadPal : Chip16 -> Number U16 -> Chip16
loadPal machine addr =
  let
    getPal : Chip16 -> Array Int
    getPal mch = case mch.graphics.palette of
      Palette sl -> sl
    set : Chip16 -> Int -> Int -> Chip16
    set mch idx color = { mch | graphics = set_palette (Palette (Array.set idx color (getPal mch))) mch.graphics }
    adder idx = (idx * 2) + to addr
  in
    List.foldl
      (\i mch -> case Memory.get (u16from (adder i)) machine.memory of
        Just color -> set mch i (bits color)
        _ -> Debug.todo "address does not exist!")
      machine
      (List.range 0 15)

opPalAddr : Chip16 -> Number U16 -> Chip16
opPalAddr machine hhll = loadPal machine hhll

opPalReg : Chip16 -> Number I8 -> Chip16
opPalReg machine rx =
  case get_rx machine.cpu rx of
    Just addr -> loadPal machine (intou16 addr)
    _ -> Debug.todo "invalid register"

not : Number I16 -> Cpu -> (Number I16, Cpu)
not v cpu = 
  let
    res = Numbers.not v
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

neg : Number I16 -> Cpu -> (Number I16, Cpu)
neg v cpu = 
  let
    res = Numbers.neg v
    flags = [(FZero, isZero res), (FNegative, isNeg res)]
  in
    (res, { cpu | flags = set_flags flags cpu.flags })

opNoti : Chip16 -> Number I8 -> Number I16 -> Chip16
opNoti machine rx hhll =
  case not hhll machine.cpu of
    (res, cpu) -> { machine
                  | cpu = set_rx cpu rx res }

opNot1 : Chip16 -> Number I8 -> Chip16
opNot1 machine rx =
  case get_rx machine.cpu rx of
    Just vx ->
      case not vx machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNot2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opNot2 machine rx ry =
  case get_rx machine.cpu ry of
    Just vy ->
      case not vy machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNegi : Chip16 -> Number I8 -> Number I16 -> Chip16
opNegi machine rx hhll = 
  case neg hhll machine.cpu of
    (res, cpu) -> { machine
                  | cpu = set_rx cpu rx res }

opNeg1 : Chip16 -> Number I8 -> Chip16
opNeg1 machine rx = 
  case get_rx machine.cpu rx of
    Just vx ->
      case neg vx machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"

opNeg2 : Chip16 -> Number I8 -> Number I8 -> Chip16
opNeg2 machine rx ry = 
  case get_rx machine.cpu ry of
    Just vy ->
      case neg vy machine.cpu of
        (res, cpu) -> { machine
                      | cpu = set_rx cpu rx res }
    _ -> Debug.todo "invalid register"


opCls : Chip16 -> Chip16
opCls machine =
 { machine | graphics = Graphics.clear (set_bg 0 (clear_fg machine.graphics)) }

opBgc : Chip16 -> Number I8 -> Chip16
opBgc machine n =
  { machine | graphics = set_bg (to n) machine.graphics }

opSpr : Chip16 -> Number I8 -> Number I8 -> Chip16
opSpr machine w h =
  { machine | graphics = set_spritewh (to w) (to h) machine.graphics }

drawRow : (Float, Float) -> Number U16 -> Chip16 -> Int -> Graphics -> Graphics
drawRow (x, y) addr machine row gfx =
  let
    idx : Int -> Number U16
    idx i = u16from (to addr + row * machine.graphics.spritew + i)

    put : Int -> Number I8 -> Float -> Graphics -> Graphics
    put i v n theGfx = if isZero v then theGfx else Graphics.putPixel (x + toFloat i * 2 + n, y + toFloat row) (to v) theGfx

    putPixels : Int -> Graphics -> Graphics
    putPixels i theGfx =
      case Memory.get8 (idx i) machine.memory of
        Just v ->
          case nibbles v of
            (ll, hh) -> (put i ll 1 (put i hh 0 theGfx))
        _ -> Debug.todo "oopse!"
  in
    List.foldl
      (\i -> putPixels i)
      gfx
      (List.range 0 (machine.graphics.spritew-1))

drawSprite : (Float, Float) -> Number U16 -> Chip16 -> Chip16
drawSprite (x, y) addr machine =
  let
    gfx = 
      List.foldl
        (drawRow (x, y) addr machine)
        machine.graphics
        (List.range 0 (machine.graphics.spriteh - 1))
  in
    { machine | graphics = gfx }

opDrwMem : Chip16 -> Number I8 -> Number I8 -> Number U16 -> Chip16
opDrwMem machine rx ry hhll =
  case get_rx2 machine.cpu rx ry of
    Just (x, y) -> drawSprite (toFloat (to x), toFloat (to y)) hhll machine
    _ -> Debug.todo "failed to draw"

opDrwReg : Chip16 -> Number I8 -> Number I8 -> Number I8 -> Chip16
opDrwReg machine rx ry rz =
    case get_rx3 machine.cpu rx ry rz of
      Just (x, y, addr) -> drawSprite (toFloat (to x), toFloat (to y)) (intou16 addr) machine
      _ -> Debug.todo "invalid address!"

opRnd : Chip16 -> Number I8 -> Number U16 -> Chip16
opRnd machine rx hhll =
  let
    gen = Random.int 0 (to hhll)
    (num, seed) = Random.step gen machine.cpu.seed
  in
    { machine | cpu = set_seed seed (set_rx machine.cpu rx (i16from num)) }

opFlip : Chip16 -> Bool -> Bool -> Chip16
opFlip machine hflip vflip =
  { machine | graphics = set_flip hflip vflip machine.graphics }

opSnd0 machine = machine
opSnd machine freq hhll = machine
opSnp machine rx hhll = machine
opSng machine ad vtsr = machine

opJmpi : Chip16 -> Number U16 -> Chip16
opJmpi machine hhll
  = { machine | cpu = set_pc machine.cpu hhll }

opJmc : Chip16 -> Number U16 -> Chip16
opJmc machine hhll =
  if machine.cpu.flags.carry then
    { machine | cpu = set_pc machine.cpu hhll }
  else
    machine

shouldJump : Int -> Flags -> Bool
shouldJump x fs =
  case x of
    0x0 -> fs.zero
    0x1 -> Basics.not fs.zero
    0x2 -> fs.negative
    0x3 -> Basics.not fs.negative
    0x4 -> (Basics.not fs.negative) && (Basics.not fs.zero)
    0x5 -> fs.overflow
    0x6 -> Basics.not fs.overflow
    0x7 -> (Basics.not fs.carry) && (Basics.not fs.zero)
    0x8 -> Basics.not fs.carry
    0x9 -> fs.carry
    0xA -> fs.carry || fs.zero
    0xB -> (fs.overflow == fs.negative) && (Basics.not fs.zero)
    0xC -> fs.overflow == fs.negative
    0xD -> fs.overflow /=  fs.negative
    0xE -> (fs.overflow /= fs.negative) || fs.zero
    _ -> Debug.todo "invalid branch type"

opJx : Chip16 -> Number I8 -> Number U16 -> Chip16
opJx machine x hhll =
  if shouldJump (to x) machine.cpu.flags then
    opJmpi machine hhll
  else
    machine

opJme : Chip16 -> Number I8 -> Number I8 -> Number U16 -> Chip16
opJme machine rx ry hhll =
  case get_rx2 machine.cpu rx ry of
    Just (vx, vy) ->
      if Numbers.eq vx vy then
        { machine | cpu = set_pc machine.cpu hhll }
      else
        machine
    _ -> Debug.todo "invalid registers"

opCalli : Chip16 -> Number U16 -> Chip16
opCalli machine hhll =
  let
    mem = Memory.set machine.cpu.sp (intoi16 machine.cpu.pc) machine.memory
    newsp = Numbers.add (u16from 2) machine.cpu.sp
  in
    { machine
    | memory = mem
    , cpu = set_sp (set_pc machine.cpu hhll) newsp }

opRet : Chip16 -> Chip16
opRet machine =
  let
    newsp = Numbers.sub machine.cpu.sp (u16from 2)
    spval = case Memory.get newsp machine.memory of
      Just val -> intou16 val
      _ -> Debug.todo "invalid address in SP"
  in
    { machine
    | cpu = set_sp (set_pc machine.cpu spval) newsp }

opJmp : Chip16 -> Number I8 -> Chip16
opJmp machine rx =
  case get_rx machine.cpu rx of
    Just vx -> { machine | cpu = set_pc machine.cpu (intou16 vx) }
    _ -> Debug.todo "invalid register"

opCx : Chip16 -> Number I8 -> Number U16 -> Chip16
opCx machine x hhll =
  if shouldJump (to x) machine.cpu.flags then
    opCalli machine hhll
  else
    machine

opCall : Chip16 -> Number I8 -> Chip16
opCall machine rx = 
  let
    mem = Memory.set machine.cpu.sp (intoi16 machine.cpu.pc) machine.memory
    newsp = Numbers.add (u16from 2) machine.cpu.sp
  in
    case get_rx machine.cpu rx of
      Just vx -> { machine
                 | memory = mem
                 , cpu = set_sp (set_pc machine.cpu (intou16 vx)) newsp }
      _ -> Debug.todo "invalid register"

type Key = Up | Down | Left | Right | A | B | Select | Start

-- key to bit index
keyMap : Key -> Int
keyMap k =
  case k of
    Up -> 0
    Down -> 1
    Left -> 2
    Right -> 3
    Select -> 4
    Start -> 5
    A -> 6
    B -> 7

controllerAddr : Number U16
controllerAddr = u16from 0xFFF0

setController : Number I16 -> Chip16 -> Chip16
setController v machine = 
  { machine | memory = Memory.set controllerAddr v machine.memory }

getController : Chip16 -> Number I16
getController machine =
  case Memory.get controllerAddr machine.memory of
    Just v -> v
    _ -> Debug.todo "impossible"


-- attempt to dispatch an instruction from 4 bytes
dispatch : Chip16 -> Bool -> Number I8 -> Number I8 -> Number I8 -> Number I8 -> Chip16
dispatch machine_ vblank a b c d =
  let
    machine = { machine_ | cpu = inc_pc 4 machine_.cpu }
    (rx, ry) = nibbles b
    (rz, _) = nibbles c
    n = intoi16 rz
    hhll = u16build c d
    imm = i16build c d
    hh = d
    ll = c
    vtsr = u16build c d
    ad = b
    (x, _) = nibbles b
  in
    case bits a of
      -- 0x Misc/Video/Audio
      0x00 -> machine
      0x01 -> opCls machine
      0x02 ->
        if vblank then
          machine
        else
          { machine | cpu = dec_pc 4 machine.cpu }
      0x03 -> opBgc machine (intoi8 n)
      0x04 -> opSpr machine ll hh
      0x05 -> opDrwMem machine rx ry hhll
      0x06 -> opDrwReg machine rx ry rz
      0x07 -> opRnd machine rx hhll
      0x08 ->
        if isZero b && isZero c then
          case to d of
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
      0x20 -> opLoad_RegImm machine rx imm
      0x21 -> opLoad_SpImm machine hhll
      0x22 -> opLoad_RegMem machine b hhll
      0x23 -> opLoad_RegReg machine rx ry
      0x24 -> opMov machine rx ry
      -- 3x Stores
      0x30 -> opStore_Imm machine rx hhll
      0x31 -> opStore_Reg machine rx ry
      -- 4x Addition
      0x40 -> opAddi machine rx imm
      0x41 -> opAdd2 machine rx ry
      0x42 -> opAdd3 machine rx ry rz
      -- 5x Subtraction
      0x50 -> opSubi machine rx imm
      0x51 -> opSub2 machine rx ry
      0x52 -> opSub3 machine rx ry rz
      0x53 -> opCmpi machine rx imm
      0x54 -> opCmp machine rx ry
      -- 6x Bitwise AND
      0x60 -> opAndi machine rx imm
      0x61 -> opAnd2 machine rx ry
      0x62 -> opAnd3 machine rx ry rz
      0x63 -> opTsti machine rx imm
      0x64 -> opTst machine rx ry
      -- 7x Bitwise OR
      0x70 -> opOri machine rx imm
      0x71 -> opOr2 machine rx ry
      0x72 -> opOr3 machine rx ry rz
      -- 8x Bitwise XOR
      0x80 -> opXori machine rx imm
      0x81 -> opXor2 machine rx ry
      0x82 -> opXor3 machine rx ry rz
      -- 9x Multiplication
      0x90 -> opMuli machine rx imm
      0x91 -> opMul2 machine rx ry
      0x92 -> opMul3 machine rx ry rz
      -- Ax Division
      0xA0 -> opDivi machine rx imm
      0xA1 -> opDiv2 machine rx ry
      0xA2 -> opDiv3 machine rx ry rz
      0xA3 -> opModi machine rx imm
      0xA4 -> opMod2 machine rx ry
      0xA5 -> opMod3 machine rx ry rz
      0xA6 -> opRemi machine rx imm
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
      0xE0 -> opNoti machine rx imm
      0xE1 -> opNot1 machine rx
      0xE2 -> opNot2 machine rx ry
      0xE3 -> opNegi machine rx imm
      0xE4 -> opNeg1 machine rx
      0xE5 -> opNeg2 machine rx ry
      _ -> Debug.todo ("instruction `" ++  Debug.toString (to a) ++ " ` does not exist!")
