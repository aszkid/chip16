module Numbers exposing (Int8, Int16, UInt8, UInt16, ChipInt (..), add)

type alias Int8 = Int
type alias Int16 = Int
type alias UInt8 = Int
type alias UInt16 = Int
type ChipInt = Int8 Int8 | Int16 Int16
    | UInt8 UInt8 | UInt16 UInt16

add : ChipInt -> ChipInt -> ChipInt
add x y =
  case (x, y) of
    (Int8 a, Int8 b) -> Debug.todo "Int8 +"
    (Int16 a, Int16 b) -> Debug.todo "Int16 +"
    (UInt8 a, UInt8 b) -> UInt8 (modBy 255 (a + b))
    (UInt16 a, UInt16 b) -> UInt16 (modBy 65535 (a + b))
    _ -> Debug.todo "cannot add different-width signed ints"
