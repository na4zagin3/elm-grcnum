module Digits exposing (Myriads, SexagesimalTriple, explodeIntoDigits, explodeIntoMyriads, explodeIntoSexagesimalTriple,
                            explodeBigIntBy, zero)

import BigInt
import Dict
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)


type alias Myriads = List (Int)
type alias SexagesimalTriple = (BigInt.BigInt, BigInt.BigInt, BigInt.BigInt)

explodeIntoDigits : Int -> List Int
explodeIntoDigits n =
   let sub acc m = if m >= 10 then sub ((modBy 10 m) :: acc) (m // 10) else m::acc
   in
   sub [] n

explodeBigIntIntoIntsBy : BigInt.BigInt -> BigInt.BigInt -> Myriads
explodeBigIntIntoIntsBy =
    let bigIntToInt m = BigInt.toString m |> String.toInt |> Maybe.withDefault 0 in
    explodeBigIntBySub bigIntToInt

explodeBigIntBy : BigInt.BigInt -> BigInt.BigInt -> List BigInt.BigInt
explodeBigIntBy =
    explodeBigIntBySub identity

explodeBigIntBySub : (BigInt.BigInt -> a) -> BigInt.BigInt -> BigInt.BigInt -> List a
explodeBigIntBySub f mod n =
    let sub acc m =
            if BigInt.gt m zero
            then let (q, r) = BigInt.divmod m mod |> Maybe.withDefault (zero, zero) in
                 sub (f r :: acc) q
            else acc
    in
    sub [] n

zero = BigInt.fromInt 0
sixty = BigInt.fromInt 60
myriad = BigInt.fromInt 10000

explodeIntoMyriads : BigInt.BigInt -> Myriads
explodeIntoMyriads = explodeBigIntIntoIntsBy myriad

explodeIntoSexagesimalTriple : BigInt.BigInt -> SexagesimalTriple
explodeIntoSexagesimalTriple n =
    let (qs, s) = BigInt.divmod n sixty |> Maybe.withDefault (zero, zero) in
    let (d, m) = BigInt.divmod qs sixty |> Maybe.withDefault (zero, zero) in
    let convToInt x = BigInt.toString x |> String.toInt |> Maybe.withDefault 0 in
    (d, m, s)
