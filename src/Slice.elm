module Slice exposing (Slice, new, fromList, get, length, set, append, push)
import Array

-- `Slice` is a typesafe fixed-length array
type Slice a = Slice Int (Array.Array a)

-- creation
new n default = Slice n (Array.initialize n (\_ -> default))
fromList l =
  let
    arr = Array.fromList l
  in
    Slice (Array.length arr) arr

-- query
get i (Slice n arr) = Array.get i arr
length (Slice n arr) = n

-- manipulate
set i val (Slice n arr) = Slice n (Array.set i val arr)
append (Slice n1 arr1) (Slice n2 arr2) = Slice (n1+n2) (Array.append arr1 arr2)
push val (Slice n arr) = Slice (n+1) (Array.push val arr)