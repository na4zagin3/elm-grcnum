module SexagesimalTripleSpec exposing (..)

import BigInt
import SexagesimalTriple
import Expect exposing (Expectation, pass, fail)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Test exposing (..)


suite : Test
suite =
    describe "SexagesimalTriple"
        [ describe "SexagesimalTriple.secondsToCommon"
              [let sg = tuple3 (int, int, int) in
               fuzz2 sg sg "return too big for all numbers which is bigger than a number" <|
                    \u v ->
                    let conv x = x |> SexagesimalTriple.secondsToCommon in
                    let zero = BigInt.fromInt 0 in
                    let base = BigInt.fromInt 3600 in
                    -- ToDo: Add a Fuzzer for BigInt
                    let convn (a, b, c) =
                          BigInt.fromInt a
                          |> BigInt.mul base
                          |> BigInt.add (BigInt.fromInt b)
                          |> BigInt.mul base
                          |> BigInt.add (BigInt.fromInt c) in
                    let n = convn u in
                    let m = convn v in
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
