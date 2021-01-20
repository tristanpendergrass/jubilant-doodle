module Ticker exposing (Outcome(..), Ticker, advance, create, getValue, toPercent)


type Ticker
    = Ticker Float


create : Ticker
create =
    Ticker 1000


getValue : Ticker -> Float
getValue (Ticker value) =
    value


type Outcome
    = NoTick Ticker
    | Ticked Ticker Int


advance : Float -> Ticker -> Outcome
advance delta (Ticker current) =
    let
        newValue =
            current - delta
    in
    if newValue >= 0 then
        NoTick (Ticker newValue)

    else
        let
            timesTicked =
                floor ((newValue / -1000) + 1)

            makePositive : Float -> Float
            makePositive num =
                if num >= 0 then
                    num

                else
                    num + 1000

            newCurrent =
                makePositive newValue
        in
        Ticked (Ticker newCurrent) timesTicked


toPercent : Ticker -> Int
toPercent (Ticker current) =
    round (1000 - current) // 10
