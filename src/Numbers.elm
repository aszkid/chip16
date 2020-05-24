module Numbers exposing 
  ( Int8, Int16, UInt8, UInt16, ChipInt (..)
  , add, sub, neg
  , buildLE, nibbleLO, nibbleHI
  , i8from, i16from, u16from
  , to, tou16, toi16
  , isNeg, isPos, isZero)

import Bitwise exposing (and, or, shiftLeftBy)

{-- opaque fixed-width int types.
    since we don't expose explicit constructors,
    any external code is forced to use our
    special constructors (i8from, etc),
    ensuring that the underlying data makes sense --}
type Int8 = Int8 Int
type Int16 = Int16 Int
type UInt8 = UInt8 Int
type UInt16 = UInt16 Int
type ChipInt = I8 Int8 | I16 Int16
    | U8 UInt8 | U16 UInt16

i8from : Int -> Int8
i8from x = Int8 (Bitwise.and 0xFF x)

i16from : Int -> Int16
i16from x = Int16 (Bitwise.and 0xFFFF x)

u16from : Int -> UInt16
u16from x = UInt16 (Bitwise.and 0xFFFF x)

-- natural representation of the given ChipInt
to : ChipInt -> Int
to x =
  case x of
    I8 (Int8 v) -> Bitwise.and 0x7F v + -1 * Bitwise.and 0x80 v
    I16 (Int16 v) -> Bitwise.and 0x7FFF v + -1 * Bitwise.and 0x8000 v
    U8 (UInt8 v) -> v
    U16 (UInt16 v) -> v

tou16 : ChipInt -> UInt16
tou16 x =
  case x of
    I8 (Int8 v) -> (UInt16 v)
    I16 (Int16 v) -> (UInt16 v)
    U8 (UInt8 v) -> (UInt16 v)
    U16 v -> v

toi16 : ChipInt -> Int16
toi16 x =
  case x of
    I8 (Int8 v) -> Debug.todo "widening i8 is unimplemented"
    I16 v -> v
    U8 (UInt8 v) -> (Int16 v)
    U16 (UInt16 v) -> (Int16 v)

add_ : Int -> Int -> Int -> (Int, Bool)
add_ x y mod =
  let
    res = x + y
    carry = res > (mod - 1)
  in
    (modBy mod res, carry)

{-- given x, y of same width, represented in 2s complement,
    return x + y in 2s complement of the same width and a carry bit --}
add : ChipInt -> ChipInt -> (ChipInt, Bool)
add x y =
  case (x, y) of
    (I8 (Int8 a), I8 (Int8 b)) ->
      case add_ a b 256 of
        (res, carry) -> (I8 (Int8 res), carry)
    (I16 (Int16 a), I16 (Int16 b)) ->
      case add_ a b 65536 of
        (res, carry) -> (I16 (Int16 res), carry)
    (U8 (UInt8 a), U8 (UInt8 b)) ->
      case add_ a b 256 of
        (res, carry) -> (U8 (UInt8 res), carry)
    (U16 (UInt16 a), U16 (UInt16 b)) ->
      case add_ a b 65536 of
        (res, carry) -> (U16 (UInt16 res), carry)
    _ -> Debug.todo "cannot add different-width ints"

neg : ChipInt -> ChipInt
neg x =
  case x of
    I8 (Int8 v) -> I8 (Int8 (and (Bitwise.complement v + 1) 0xFF))
    I16 (Int16 v) -> I16 (Int16 (and (Bitwise.complement v + 1) 0xFFFF))
    _ -> Debug.todo "no can do"

sub : ChipInt -> ChipInt -> (ChipInt, Bool)
sub x y =
  case (x, y, neg y) of
    (I8 (Int8 a), I8 (Int8 b), I8 (Int8 bneg)) ->
      case add_ a bneg 256 of
        (res, _) -> (I8 (Int8 res), b > a)
    (I16 (Int16 a), I16 (Int16 b), I16 (Int16 bneg)) ->
      case add_ a bneg 65536 of
        (res, _) -> (I16 (Int16 res), b > a)
    _ -> Debug.todo "no can do yet"

isNeg : ChipInt -> Bool
isNeg n = to n < 0

isPos : ChipInt -> Bool
isPos n = to n > 0

isZero : ChipInt -> Bool
isZero n = to n == 0


buildLE : Int8 -> Int8 -> UInt16
buildLE (Int8 low) (Int8 hi) = UInt16 (or (shiftLeftBy 8 hi) low)

nibbleLO : Int8 -> Int8
nibbleLO (Int8 b) = Int8 (and b 0xF)

nibbleHI : Int8 -> Int8
nibbleHI (Int8 b) = Int8 (Bitwise.shiftRightBy 4 (and b 0xF0))