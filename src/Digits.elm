module Digits exposing (Myriads, explodeIntoDigits, explodeIntoMyriads)

import BigInt
import Dict
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)


type alias Myriads = List (Int)

explodeIntoDigits : Int -> List Int
explodeIntoDigits n =
   let sub acc m = if m >= 10 then sub ((modBy 10 m) :: acc) (m // 10) else m::acc
   in
   sub [] n

explodeIntoMyriads : BigInt.BigInt -> Myriads
explodeIntoMyriads n =
    let myriad = BigInt.fromInt 10000 in
    let zero = BigInt.fromInt 0 in
    let bigIntToInt m = BigInt.toString m |> String.toInt |> Maybe.withDefault 0 in
    let sub acc m =
            if BigInt.gt m zero
            then let (q, r) = BigInt.divmod m myriad |> Maybe.withDefault (zero, zero) in
                 let ir = bigIntToInt r in
                 sub (ir :: acc) q
            else acc
    in
    sub [] n
