module Memory exposing (Memory, init)

import Numbers exposing (Int16)
import Slice exposing (Slice)

type Memory = Memory (Slice Int16)

init = Memory (Slice.new 4096 0)