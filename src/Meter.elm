module Meter exposing (Meter, create, decrement, getMax, getValue, increment)


type alias Max =
    Int


type alias Current =
    Int


type Meter
    = Meter Current Max


create : Int -> Meter
create max =
    Meter max max


decrement : Int -> Meter -> Meter
decrement amount (Meter current max) =
    Meter (clamp 0 max (current - amount)) max


increment : Int -> Meter -> Meter
increment amount (Meter current max) =
    Meter (clamp 0 max (current + amount)) max


getValue : Meter -> Int
getValue (Meter current _) =
    current


getMax : Meter -> Int
getMax (Meter _ max) =
    max
