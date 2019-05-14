module SexagesimalSpec exposing (..)

import BigInt
import Sexagesimal
import Expect exposing (Expectation, pass, fail)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Test exposing (..)
import Parser

smallInt =
    Fuzz.intRange -30 30

bigIntList =
    let fuzzDigit = (Fuzz.intRange 0 300) in
    let f n = BigInt.fromInt n in
    Fuzz.map (\x -> x |> List.take 100 |> List.map f) (list (Fuzz.intRange 0 300))

sexagesimal =
    Fuzz.map2 Sexagesimal.Sexagesimal bigIntList bigIntList

suite : Test
suite =
    describe "Sexagesimal"
        [ describe "Sexagesimal.normalize"
              [fuzz sexagesimal "must be idempotent" <|
                   \x->
                   let result1 = Sexagesimal.normalize x in
                   let result2 = Sexagesimal.normalize result1 in
                   Expect.equal result1 result2
              ,fuzz sexagesimal "preserves the value" <|
                   \x->
                   let result1 = Sexagesimal.toBigInt x in
                   let result2 = Sexagesimal.toBigInt (Sexagesimal.normalize x) in
                   Expect.equal result1 result2
              ,fuzz sexagesimal "convert" <|
                   \x->
                   let lessThan60 n = BigInt.lt n (BigInt.fromInt 60) in
                   let atLeast0 n = BigInt.gte n (BigInt.fromInt 0) in
                   let result = Sexagesimal.normalize x in
                   Expect.all
                       [ \v -> Sexagesimal.integers v |> List.filter (\n -> lessThan60 n |> not) |> Expect.equalLists []
                       , \v -> Sexagesimal.fractions v |> List.filter (\n -> lessThan60 n |> not) |> Expect.equalLists []
                       ] result
              ]
        , describe "Sexagesimal.toBigIntWithCoeff"
              [fuzz sexagesimal "is a valuation" <|
                   \x->
                   let normalized = Sexagesimal.normalize x in
                   Expect.equal (Sexagesimal.toBigIntWithCoeff x) (Sexagesimal.toBigIntWithCoeff normalized)
              ]
        , describe "Sexagesimal.fromString"
              [fuzz sexagesimal "must be a retraction" <|
                   \x->
                   let result = Sexagesimal.fromString (Sexagesimal.toString x) in
                   Expect.equal (Just (Sexagesimal.trimZeros x)) (Maybe.map Sexagesimal.trimZeros result)

              ]
        , describe "Sexagesimal.shiftDigits"
              [fuzz sexagesimal "shifting 1 digits means multiplying by 60" <|
                   \x->
                   let x1 = Sexagesimal.shiftDigits 1 x in
                   let xv = Sexagesimal.toBigInt x in
                   case xv of
                       Nothing -> pass
                       Just _ -> Expect.equal
                                 (Maybe.map (BigInt.mul (BigInt.fromInt 60)) xv)
                                 (Sexagesimal.toBigInt x1)
              ,fuzz sexagesimal "must be an identity if shift amount is zero" <|
                   \x->
                   let result = Sexagesimal.shiftDigits 0 x in
                   Expect.equal (Sexagesimal.trimZeros x) (Sexagesimal.trimZeros result)
              ,fuzz2 sexagesimal smallInt "must have the inverse" <|
                   \x n->
                   let result = Sexagesimal.shiftDigits (-n) (Sexagesimal.shiftDigits n x) in
                   Expect.equal (Sexagesimal.trimZeros x) (Sexagesimal.trimZeros result)
              ,fuzz3 sexagesimal smallInt smallInt "must be a composable" <|
                   \x a b->
                   let result1 = Sexagesimal.shiftDigits b (Sexagesimal.shiftDigits a x) in
                   let result2 = Sexagesimal.shiftDigits (a + b) x in
                   Expect.equal (Sexagesimal.trimZeros result1) (Sexagesimal.trimZeros result2)
              ]
        , describe "Sexagesimal.parseSexagesimal"
              [fuzz sexagesimal "must be a retraction" <|
                   \x->
                   let result = Parser.run Sexagesimal.parseSexagesimal (Sexagesimal.toString x) in
                   Expect.equal (Ok (Sexagesimal.trimZeros x)) (Result.map Sexagesimal.trimZeros result)

              ]
        ]
