module TickerTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Ticker exposing (Ticker)


toTicker : Ticker.Outcome -> Ticker
toTicker outcome =
    case outcome of
        Ticker.NoTick ticker ->
            ticker

        Ticker.Ticked ticker _ ->
            ticker


isNoTick : Ticker.Outcome -> Expectation
isNoTick outcome =
    case outcome of
        Ticker.NoTick _ ->
            Expect.pass

        Ticker.Ticked _ _ ->
            Expect.fail "Outcome not NoTick"


isTicked : Ticker.Outcome -> Expectation
isTicked outcome =
    case outcome of
        Ticker.NoTick _ ->
            Expect.fail "Outcome not Ticked"

        Ticker.Ticked _ _ ->
            Expect.pass


isTickedNTimes : Int -> Ticker.Outcome -> Expectation
isTickedNTimes amount outcome =
    case outcome of
        Ticker.NoTick _ ->
            Expect.fail "Outcome not Ticked"

        Ticker.Ticked _ timesTicked ->
            Expect.equal amount timesTicked


expectCloseTo : Float -> Float -> Expectation
expectCloseTo =
    Expect.within (Expect.Absolute 0.000000001)


suite : Test
suite =
    describe "The Ticker module"
        [ test "Can create a ticker with starting value 1000" <|
            \_ ->
                Ticker.create
                    |> Ticker.getValue
                    |> expectCloseTo 1000
        , test "Has the right value after an advance" <|
            \_ ->
                Ticker.create
                    |> Ticker.advance 500
                    |> toTicker
                    |> Ticker.getValue
                    |> expectCloseTo 500
        , test "Has the right value after an advance of 1000" <|
            \_ ->
                Ticker.create
                    |> Ticker.advance 1000
                    |> toTicker
                    |> Ticker.getValue
                    |> expectCloseTo 0
        , test "Has outcome NoTick when no tick" <|
            \_ ->
                Ticker.create
                    |> Ticker.advance 500
                    |> isNoTick
        , test "Has outcome Ticked when ticked" <|
            \_ ->
                Ticker.create
                    |> Ticker.advance 1050
                    |> isTicked
        , test "Has correct number of ticks when ticked" <|
            \_ ->
                Ticker.create
                    |> Ticker.advance 5000
                    |> isTickedNTimes 5
        , describe "toPercent function"
            [ test "Works for 0%" <|
                \_ ->
                    Ticker.create
                        |> Ticker.toPercent
                        |> Expect.equal 0
            , test "Works for 1%" <|
                \_ ->
                    Ticker.create
                        |> Ticker.advance 10
                        |> toTicker
                        |> Ticker.toPercent
                        |> Expect.equal 1
            , test "Works for 50%" <|
                \_ ->
                    Ticker.create
                        |> Ticker.advance 500
                        |> toTicker
                        |> Ticker.toPercent
                        |> Expect.equal 50
            , test "Works for 99%" <|
                \_ ->
                    Ticker.create
                        |> Ticker.advance 999
                        |> toTicker
                        |> Ticker.toPercent
                        |> Expect.equal 99
            ]
        ]
