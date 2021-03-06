module Numbers2 exposing
  ( U8, I8, U16, I16
  , Number
  , to, bits
  , u8from, u16from, i8from, i16from
  , intou8, intou16, intoi8, intoi16
  , unpacki16, i16build, u16build, nibbles, isZero, isPos, isNeg, eq
  , add, addC, neg, sub, subC, mul, mulC, div, divC, mod, rem
  , and, or, xor, Shift(..), shl, shr, not )

import Bitwise

------------------------------------------------------------------
--- TYPEDEFS
------------------------------------------------------------------

type U8 = U8 Int
type I8 = I8 Int
type U16 = U16 Int
type I16 = I16 Int

type alias NumberI a = 
  { into : a -> Int
  , from : Int -> a
  , carry : a -> a -> (Int -> Int -> Int) -> Bool
  , neg : a -> a
  , borrow : a -> a -> Bool
  , bits : a -> Int
  , width : Int }
type Number a = Number (NumberI a) a

to : Number a -> Int
to (Number intf v) = intf.into v

from : NumberI a -> Int -> Number a
from intf = \v -> Number intf (intf.from v)

bits : Number a -> Int
bits (Number intf v) = intf.bits v

------------------------------------------------------------------
--- INTEGER TYPES
------------------------------------------------------------------

u8interface : NumberI U8
u8interface =
  { into = \(U8 v) -> v
  , from = \i -> U8 (Bitwise.and 0xFF i)
  , carry = \(U8 va) (U8 vb) op -> op va vb > 255
  , neg = \(U8 v) -> U8 (Bitwise.and (Bitwise.complement v + 1) 0xFF)
  , borrow = \(U8 va) (U8 vb) -> vb > va
  , bits = \(U8 v) -> v
  , width = 8 }

u8from : Int -> Number U8
u8from = from u8interface

intou8 : Number a -> Number U8
intou8 (Number intfa va) = intfa.bits va |> u8from

u16interface : NumberI U16
u16interface =
  { into = \(U16 v) -> v
  , from = \i -> U16 (Bitwise.and 0xFFFF i)
  , carry = \(U16 va) (U16 vb) op -> op va vb > 65535
  , neg = \(U16 v) -> U16 (Bitwise.and (Bitwise.complement v + 1) 0xFFFF)
  , borrow = \(U16 va) (U16 vb) -> vb > va
  , bits = \(U16 v) -> v
  , width = 16 }

u16from : Int -> Number U16
u16from = from u16interface

intou16 : Number a -> Number U16
intou16 (Number intfa va) = intfa.bits va |> u16from

i8interface : NumberI I8
i8interface = 
  { into = \(I8 v) -> Bitwise.and 0x7F v + -1 * Bitwise.and 0x80 v
  , from = \i -> I8 (Bitwise.and 0xFF i)
  , carry = \(I8 va) (I8 vb) op -> op va vb > 255
  , neg = \(I8 v) -> I8 (Bitwise.and (Bitwise.complement v + 1) 0xFF)
  , borrow = \(I8 va) (I8 vb) -> vb > va
  , bits = \(I8 v) -> v
  , width = 8 }

i8from : Int -> Number I8
i8from = from i8interface

intoi8 : Number a -> Number I8
intoi8 (Number intfa va) = intfa.bits va |> i8from

i16interface : NumberI I16
i16interface = 
  { into = \(I16 v) -> Bitwise.and 0x7FFF v + -1 * Bitwise.and 0x8000 v
  , from = \i -> I16 (Bitwise.and 0xFFFF i)
  , carry = \(I16 va) (I16 vb) op -> op va vb > 65535
  , neg = \(I16 v) -> I16 (Bitwise.and (Bitwise.complement v + 1) 0xFFFF)
  , borrow = \(I16 va) (I16 vb) -> vb > va
  , bits = \(I16 v) -> v
  , width = 16 }

i16from : Int -> Number I16
i16from = from i16interface

intoi16 : Number a -> Number I16
intoi16 (Number intfa va) = intfa.bits va |> i16from

------------------------------------------------------------------
--- ARITHMETIC
------------------------------------------------------------------

add : Number a -> Number a -> Number a
add (Number intf va) (Number _ vb) = Number intf (intf.from (intf.bits va + intf.bits vb))

addC : Number a -> Number a -> (Number a, Bool)
addC x y =
  case (x, y) of
    (Number intf va, Number _ vb) -> (add x y, intf.carry va vb (+))

neg : Number a -> Number a
neg (Number intf v) = Number intf (intf.neg v)

sub : Number a -> Number a -> Number a
sub x (Number intf y) = add x (Number intf (intf.neg y))

subC : Number a -> Number a -> (Number a, Bool)
subC x y =
  case (x, y) of
    (Number intf vx, Number _ vy) -> (sub x y, intf.borrow vx vy)

mul : Number a -> Number a -> Number a
mul (Number intf va) (Number _ vb) = Number intf (intf.from (intf.bits va * intf.bits vb))

mulC : Number a -> Number a -> (Number a, Bool)
mulC x y =
  case (x, y) of
    (Number intf vx, Number _ vy) -> (mul x y, intf.carry vx vy (*))

div : Number a -> Number a -> Number a
div (Number intf va) (Number _ vb) = Number intf (intf.from (intf.bits va // intf.bits vb))

divC : Number a -> Number a -> (Number a, Bool)
divC x y =
  case (x, y) of
    (Number intf vx, Number _ vy) -> (div x y, remainderBy (intf.bits vy) (intf.bits vx) /= 0)

mod : Number a -> Number a -> Number a
mod (Number intf va) (Number _ vb) = Number intf (intf.from (modBy (intf.bits vb) (intf.bits va)))

rem : Number a -> Number a -> Number a
rem (Number intf va) (Number _ vb) = Number intf (intf.from (remainderBy (intf.bits vb) (intf.bits va)))

------------------------------------------------------------------
--- BITWISE
------------------------------------------------------------------

and : Number a -> Number a -> Number a
and (Number intf va) (Number _ vb) = Number intf (intf.from (Bitwise.and (intf.bits va) (intf.bits vb)))

or : Number a -> Number a -> Number a
or (Number intf va) (Number _ vb) = Number intf (intf.from (Bitwise.or (intf.bits va) (intf.bits vb)))

xor : Number a -> Number a -> Number a
xor (Number intf va) (Number _ vb) = Number intf (intf.from (Bitwise.xor (intf.bits va) (intf.bits vb)))

type Shift = ShiftArithmetic | ShiftLogical

shl : Number a -> Number a -> Number a
shl (Number intf x) (Number _ by) = Number intf (intf.from (Bitwise.shiftLeftBy (intf.bits by) (intf.bits x)))

shr : Number a -> Number a -> Shift -> Number a
shr (Number intf x) (Number _ by) t =
  let
    fill = 32 - intf.width
  in
    case t of
        ShiftArithmetic -> Number intf (intf.from (Bitwise.shiftRightBy (intf.bits by + 32 - intf.width) (Bitwise.shiftLeftBy fill (intf.bits x))))
        ShiftLogical -> Number intf (intf.from (Bitwise.shiftRightZfBy (intf.bits by) (intf.bits x)))

not : Number a -> Number a
not (Number intf v) = Number intf (intf.from (Bitwise.complement (intf.bits v)))

------------------------------------------------------------------
--- HELPERS
------------------------------------------------------------------

u16build : Number I8 -> Number I8 -> Number U16
u16build (Number intf low) (Number _ hi) = u16from (Bitwise.or (intf.bits low) (Bitwise.shiftLeftBy 8 (intf.bits hi)))

unpacki16 : Number I16 -> (Number I8, Number I8)
unpacki16 v = (intoi8 (and v (i16from 0xFF)), intoi8 (and (shr v (i16from 8) ShiftLogical) (i16from 0xFF)))

i16build : Number I8 -> Number I8 -> Number I16
i16build lo hi = intoi16 (u16build lo hi)

nibbles : Number I8 -> (Number I8, Number I8)
nibbles byte = (and byte (i8from 0xF), and (shr byte (i8from 4) ShiftLogical) (i8from 0xF))

isNeg : Number a -> Bool
isNeg (Number intf v) = intf.into v < 0

isPos : Number a -> Bool
isPos (Number intf v) = intf.into v > 0

isZero : Number a -> Bool
isZero (Number intf v) = intf.bits v == 0

eq : Number a -> Number a -> Bool
eq (Number intf x) (Number _ y) = intf.bits x == intf.bits y