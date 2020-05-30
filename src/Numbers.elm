module Numbers exposing 
  ( Int8, Int16, UInt8, UInt16, ChipInt (..), Shift (..)
  , add, sub, neg, mul, div, mod, rem, and, or, xor, shl, shr, not
  , add__, mul__, div__
  , buildLE, buildLEs, nibbleLO, nibbleHI, unpackLE
  , i8from, i16from, u16from
  , to, tou16, toi16, tobits
  , isNeg, isPos, isZero, eq)

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

tobits : ChipInt -> Int
tobits n =
  case n of
    I8 (Int8 v) -> v
    I16 (Int16 v) -> v
    U8 (UInt8 v) -> v
    U16 (UInt16 v) -> v

toi16 : ChipInt -> Int16
toi16 x =
  case x of
    I8 (Int8 v) -> Int16 (Bitwise.and 0xFFFF (Bitwise.shiftRightBy 24 (Bitwise.shiftLeftBy 24 v)))
    I16 v -> v
    U8 (UInt8 v) -> (Int16 v)
    U16 (UInt16 v) -> (Int16 v)

add__ : ChipInt -> ChipInt -> ChipInt
add__ x y = 
  case add x y of
    (r, _) -> r

add_ : Int -> Int -> Int -> (Int, Bool)
add_ x y modulo =
  let
    res = x + y
    carry = res > (modulo - 1)
  in
    (modBy modulo res, carry)

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
    I8 (Int8 v) -> I8 (Int8 (Bitwise.and (Bitwise.complement v + 1) 0xFF))
    I16 (Int16 v) -> I16 (Int16 (Bitwise.and (Bitwise.complement v + 1) 0xFFFF))
    U16 (UInt16 v) -> neg (I16 (i16from (to x)))
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
    (U16 (UInt16 a), U16 (UInt16 b), I16 (Int16 bneg)) ->
      case add_ a bneg 65536 of
        (res, _) -> (U16 (UInt16 res), b > a)
    _ -> Debug.todo "no can do yet"


mul__ : ChipInt -> ChipInt -> ChipInt
mul__ x y = 
  case mul x y of
    (r, _) -> r

mul_ : Int -> Int -> Int -> (Int, Bool)
mul_ x y modulo =
  let
    res = x * y
    carry = res > (modulo - 1)
  in
    (modBy modulo res, carry)

-- given x, y return x * y and carry boolean
mul : ChipInt -> ChipInt -> (ChipInt, Bool)
mul x y =
  case (x, y) of
    (I8 (Int8 a), I8 (Int8 b)) ->
      case mul_ a b 256 of
        (res, carry) -> (I8 (Int8 res), carry)
    (I16 (Int16 a), I16 (Int16 b)) ->
      case mul_ a b 65536 of
        (res, carry) -> (I16 (Int16 res), carry)
    _ -> Debug.todo "no can do yet"

div__ : ChipInt -> ChipInt -> ChipInt
div__ x y = 
  case div x y of
    (r, _) -> r

div_ : Int -> Int -> (Int, Bool)
div_ x y =
  let
    res = x // y
    remainder = remainderBy y x /= 0
  in
    (res, remainder)

-- given x, y return x / y and remainder!=0
div : ChipInt -> ChipInt -> (ChipInt, Bool)
div x y = 
  case (x, y) of
    (I8 (Int8 a), I8 (Int8 b)) ->
      case div_ a b of
        (res, remainder) -> (I8 (Int8 res), remainder)
    (I16 (Int16 a), I16 (Int16 b)) ->
      case div_ a b of
        (res, remainder) -> (I16 (Int16 res), remainder)
    _ -> Debug.todo "no can do yet"

mod : ChipInt -> ChipInt -> ChipInt
mod x by =
  case (x, by) of
    (I8 (Int8 a), I8 (Int8 b)) -> I8 (Int8 (modBy b a))
    (I16 (Int16 a), I16 (Int16 b)) -> I16 (Int16 (modBy b a))
    _ -> Debug.todo "no can do yet"

rem : ChipInt -> ChipInt -> ChipInt
rem x by =
  case (x, by) of
    (I8 (Int8 a), I8 (Int8 b)) -> I8 (Int8 (remainderBy b a))
    (I16 (Int16 a), I16 (Int16 b)) -> I16 (Int16 (remainderBy b a))
    _ -> Debug.todo "no can do yet"

and : ChipInt -> ChipInt -> ChipInt
and x y =
  case (x, y) of
    (I16 (Int16 a), I16 (Int16 b)) -> I16 (Int16 (Bitwise.and a b))
    _ -> Debug.todo "and unimpl"

or : ChipInt -> ChipInt -> ChipInt
or x y =
  case (x, y) of
    (I16 (Int16 a), I16 (Int16 b)) -> I16 (Int16 (Bitwise.or a b))
    _ -> Debug.todo "or unimpl"

xor : ChipInt -> ChipInt -> ChipInt
xor x y =
  case (x, y) of
    (I16 (Int16 a), I16 (Int16 b)) -> I16 (Int16 (Bitwise.xor a b))
    _ -> Debug.todo "xor unimpl"

type Shift = ShiftArithmetic | ShiftLogical

shl : ChipInt -> ChipInt -> ChipInt
shl x y =
  let
    shl_ mby shby val = modBy mby (Bitwise.shiftLeftBy shby val)
  in
    case (x, y) of
      (I16 (Int16 val), I16 (Int16 by)) -> I16 (Int16 (shl_ 65536 by val))
      _ -> Debug.todo "shl unimpl"

shr_ : Shift -> Int -> Int -> Int -> Int
shr_ t width shby val =
  let
    mby = Bitwise.shiftLeftBy width 1
    fill = 32 - width
  in
    modBy mby
      ( case t of
        ShiftArithmetic -> Bitwise.shiftRightBy (shby + fill) (Bitwise.shiftLeftBy fill val)
        ShiftLogical -> Bitwise.shiftRightZfBy shby val )

shr : ChipInt -> ChipInt -> Shift -> ChipInt
shr x y t =
  case (x, y) of
    (I8 (Int8 val), I8 (Int8 by)) -> I8 (Int8 (shr_ t 8 by val))
    (I16 (Int16 val), I16 (Int16 by)) -> I16 (Int16 (shr_ t 16 by val))
    _ -> Debug.todo "shr unimpl"

not : ChipInt -> ChipInt
not n =
  let
    doNot v = Bitwise.complement v
  in
    case n of
      I8 (Int8 v) -> I8 (Int8 (doNot v))
      I16 (Int16 v) -> I16 (Int16 (doNot v))
      U8 (UInt8 v) -> U8 (UInt8 (doNot v))
      U16 (UInt16 v) -> U16 (UInt16 (doNot v))

isNeg : ChipInt -> Bool
isNeg n = to n < 0

isPos : ChipInt -> Bool
isPos n = to n > 0

isZero : ChipInt -> Bool
isZero n = to n == 0

eq : ChipInt -> ChipInt -> Bool
eq x y = to x == to y

buildLE : Int8 -> Int8 -> UInt16
buildLE (Int8 low) (Int8 hi) = UInt16 (Bitwise.or (shiftLeftBy 8 hi) low)

buildLEs : Int8 -> Int8 -> Int16
buildLEs (Int8 low) (Int8 hi) = Int16 (Bitwise.or (shiftLeftBy 8 hi) low)

unpackLE : Int16 -> (Int8, Int8)
unpackLE (Int16 val) =
  (Int8 (Bitwise.and 0xFF val), Int8 (Bitwise.shiftRightZfBy 4 val)) -- LL, HH

nibbleLO : Int8 -> Int8
nibbleLO (Int8 b) = Int8 (Bitwise.and b 0xF)

nibbleHI : Int8 -> Int8
nibbleHI (Int8 b) = Int8 (Bitwise.shiftRightBy 4 (Bitwise.and b 0xF0))