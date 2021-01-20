module MeterTest exposing (..)

import Expect
import Meter exposing (Meter)
import Test exposing (..)


suite : Test
suite =
    describe "The Meter module"
        [ test "Creates a meter with a value" <|
            \_ ->
                Meter.create 100
                    |> Meter.getValue
                    |> Expect.equal 100
        , test "Creates a meter with a max" <|
            \_ ->
                Meter.create 100
                    |> Meter.getMax
                    |> Expect.equal 100
        , test "Cannot increment above the max" <|
            \_ ->
                Meter.create 100
                    |> Meter.increment 10
                    |> Meter.getValue
                    |> Expect.equal 100
        , test "Can decrement value" <|
            \_ ->
                Meter.create 100
                    |> Meter.decrement 10
                    |> Meter.getValue
                    |> Expect.equal 90
        , test "Cannot decrement value below 0" <|
            \_ ->
                Meter.create 100
                    |> Meter.decrement 500
                    |> Meter.getValue
                    |> Expect.equal 0
        ]
