module FractionSpec exposing (..)

import BigInt
import Fraction
import Expect exposing (Expectation, pass, fail)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Test exposing (..)
import Parser

posBigInt =
    Fuzz.intRange 0 3000
    |> Fuzz.map BigInt.fromInt

bigIntList =
    let fuzzDigit = (Fuzz.intRange 0 300) in
    let f n = BigInt.fromInt n in
    Fuzz.map (\x -> x |> List.take 100 |> List.map f) (list (Fuzz.intRange 0 300))

simpleFrac =
    Fuzz.map2 Fraction.SimpleFrac posBigInt posBigInt
unities =
    Fuzz.map3 Fraction.Unities posBigInt posBigInt (list posBigInt)
frac =
    Fuzz.oneOf[simpleFrac, unities]

suite : Test
suite =
    describe "Fraction"
        [ describe "Fraction.fromString"
              [fuzz frac "is a retraction" <|
                   \x->
                   let str = Fraction.toString x in
                   let result = Fraction.fromString str in
                   Expect.equal (Just x) result
              ]
        ]
