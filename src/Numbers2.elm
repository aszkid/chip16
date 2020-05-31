module Numbers2 exposing
  ( U8, I8, U16, I16
  , Number
  , to
  , u8from, u16from, i8from, i16from
  , intou8, intou16, intoi8, intoi16
  , add, addC)

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
  , carry : a -> a -> Bool }
type Number a = Number (NumberI a) a

to : Number a -> Int
to (Number intf v) = intf.into v

from : NumberI a -> Int -> Number a
from intf = \v -> Number intf (intf.from v)

------------------------------------------------------------------
--- INTEGER TYPES
------------------------------------------------------------------

u8interface : NumberI U8
u8interface =
  { into = \(U8 v) -> v
  , from = \i -> U8 (Bitwise.and 0xFF i)
  , carry = \(U8 va) (U8 vb) -> (va + vb) > 255 }

u8from : Int -> Number U8
u8from = from u8interface

intou8 : Number a -> Number U8
intou8 (Number intfa va) = intfa.into va |> u8from

u16interface : NumberI U16
u16interface =
  { into = \(U16 v) -> v
  , from = \i -> U16 (Bitwise.and 0xFFFF i)
  , carry = \(U16 va) (U16 vb) -> (va + vb) > 65535 }

u16from : Int -> Number U16
u16from = from u16interface

intou16 : Number a -> Number U16
intou16 (Number intfa va) = intfa.into va |> u16from

i8interface : NumberI I8
i8interface = 
  { into = \(I8 v) -> Bitwise.and 0x7F v + -1 * Bitwise.and 0x80 v
  , from = \i -> I8 (Bitwise.and 0xFF i)
  , carry = \(I8 va) (I8 vb) -> (va + vb) > 255 }

i8from : Int -> Number I8
i8from = from i8interface

intoi8 : Number a -> Number I8
intoi8 (Number intfa va) = intfa.into va |> i8from

i16interface : NumberI I16
i16interface = 
  { into = \(I16 v) -> Bitwise.and 0x7FFF v + -1 * Bitwise.and 0x8000 v
  , from = \i -> I16 (Bitwise.and 0xFFFF i)
  , carry = \(I16 va) (I16 vb) -> (va + vb) > 65535 }

i16from : Int -> Number I16
i16from = from i16interface

intoi16 : Number a -> Number I16
intoi16 (Number intfa va) = intfa.into va |> i16from

------------------------------------------------------------------
--- ARITHMETIC
------------------------------------------------------------------

add : Number a -> Number a -> Number a
add (Number intf va) (Number _ vb) =
  Number intf (intf.from (intf.into va + intf.into vb))

addC : Number a -> Number a -> (Number a, Bool)
addC x y =
  case (x, y) of
    (Number intf va, Number _ vb) -> (add x y, intf.carry va vb)