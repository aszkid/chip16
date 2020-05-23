module Slice exposing (Slice, new, get, length, set)
import Array

-- `Slice` is a typesafe fixed-length array
type Slice a = Slice Int (Array.Array a)

-- creation
new n default = Slice n (Array.initialize n (\_ -> default))

-- query
get i (Slice n arr) = Array.get i arr
length (Slice n arr) = n

-- manipulate
set i val (Slice n arr) = Slice n (Array.set i val arr)