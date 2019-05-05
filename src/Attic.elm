module Attic exposing (AtticSymbols, toAttic, generalSymbols)

import BigInt
import Digits exposing (..)
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)

type alias AtticSymbols = List (String, String)

toAttic : AtticSymbols -> BigInt.BigInt -> Maybe (Html msg)
toAttic ss num =
    case String.toInt (BigInt.toString num) of
        Nothing -> Nothing
        Just n ->
            let f ts = String.join "" ts |> text in
            convert ss (explodeIntoDigits n |> List.reverse) []
            |> Maybe.map f

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
