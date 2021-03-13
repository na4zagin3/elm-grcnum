module Greek.Attic exposing (AtticSymbols, toAttic, generalSymbols)

import BigInt
import Digits exposing (..)
import Prim exposing (..)

type alias AtticSymbols = List (String, String)

big1 = BigInt.fromInt 1
big2 = BigInt.fromInt 2
big31 = BigInt.fromInt 31
intMax = BigInt.sub (BigInt.pow big2 big31) big1

toAttic : AtticSymbols -> BigInt.BigInt -> Maybe (List Element)
toAttic ss num =
    let convFromInt n =
            let f ts = [String.join "" ts |> Word] in
            convert ss (explodeIntoDigits n |> List.reverse) []
            |> Maybe.map f in
    if BigInt.gt num intMax || BigInt.gte zero num
    then Nothing
    else String.toInt (BigInt.toString num)
        |> Maybe.andThen convFromInt

convert ss ds acc =
    case (ss, ds) of
        (_, []) -> List.concat acc |> Just
        ((m, p)::ssr, d::dsr) ->
            let fives = List.repeat (d // 5) p in
            let ones = List.repeat (modBy 5 d) m in
            convert ssr dsr (fives::ones::acc)
        (_, _) ->
            Nothing

generalSymbols = [("Ι", "Π"), ("Δ", "𐅄"), ("Η", "𐅅"), ("Χ", "𐅆"), ("Μ", "𐅇")]
