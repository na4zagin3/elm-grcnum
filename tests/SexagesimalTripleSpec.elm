module SexagesimalTripleSpec exposing (..)

import BigInt
import SexagesimalTriple
import Expect exposing (Expectation, pass, fail)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Test exposing (..)

zero = BigInt.fromInt 0

bigint =
    let base = BigInt.fromInt 3600 in
    let int3600 =
            Fuzz.intRange 0 3600 in
    let convn (a, b, c) =
            BigInt.fromInt a
                |> BigInt.mul base
                |> BigInt.add (BigInt.fromInt b)
                |> BigInt.mul base
                |> BigInt.add (BigInt.fromInt c) in
    tuple3 (int3600, int3600, int3600) |> Fuzz.map convn

suite : Test
suite =
    describe "SexagesimalTriple"
        [ describe "SexagesimalTriple.secondsToCommon"
              [fuzz2 bigint bigint "return too big for all numbers which is bigger than a number" <|
                    \n m ->
                    let conv x = x |> SexagesimalTriple.secondsToCommon in
                    let a = BigInt.min n m in
                    let b = BigInt.max n m in
                    case (BigInt.compare a zero, conv a, conv b) of
                        (LT, _, _) -> pass
                        (_, Just _, Just _) -> pass
                        (_, Nothing, Just _) -> fail "not monotonic"
                        (_, Just _, Nothing) -> pass
                        (_, Nothing, Nothing) -> pass
              ]

        ]
