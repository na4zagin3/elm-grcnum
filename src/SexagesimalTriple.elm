module SexagesimalTriple exposing (toCommon, secondsToCommon, secondsToPtolemy)

import Ionian
import BigInt
import Digits exposing (..)
import Dict
import Ionian
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)

zero = BigInt.fromInt 0
one = BigInt.fromInt 1

secondsToCommon : BigInt.BigInt -> Maybe (Html msg)
secondsToCommon n =
    Digits.explodeIntoSexagesimalTriple n
    |> toCommon

secondsToPtolemy : BigInt.BigInt -> Maybe (Html msg)
secondsToPtolemy n =
    Digits.explodeIntoSexagesimalTriple n
    |> toPtolemy

toCommon : SexagesimalTriple -> Maybe (Html msg)
toCommon (d, m, s) =
    let (ds, dext) = Ionian.toMyriads d in
    let (ms, mext) = Ionian.toMyriads m in
    let (ss, sext) = Ionian.toMyriads s in
    let triple =
            if dext || mext || sext
            then Nothing
            else [ds, ms, ss] |> insertSpace |> Just in
    case (BigInt.compare d one, BigInt.compare m one, BigInt.compare s one) of
        (LT, LT, LT) -> text "" |> Just
        (LT, LT, EQ) -> [deuteron, hexecostonF] |> insertSpace |> Just
        (LT, LT, GT) -> [deutera, hexecosta, ss] |> insertSpace |> Just
        (LT, EQ, LT) -> [proton, hexecostonF] |> insertSpace |> Just
        (LT, GT, LT) -> [prota, hexecosta, ms] |> insertSpace |> Just
        (LT, EQ, EQ) -> [proton, hexecoston, cai, deuteron, hexecostonF] |> insertSpace |> Just
        (LT, EQ, GT) -> [proton, hexecoston, cai, deutera, ss] |> insertSpace |> Just
        (LT, GT, EQ) -> [prota, hexecoston, ms, cai, deutera, ss] |> insertSpace |> Just
        (LT, GT, GT) -> [prota, hexecoston, ms, cai, deutera, ss] |> insertSpace |> Just
        (EQ, LT, LT) -> [ds] |> insertSpace |> Just
        (GT, LT, LT) -> [ds] |> insertSpace |> Just
        (EQ, LT, EQ) -> [moira, cai, deuteron, hexecostonF] |> insertSpace |> Just
        (EQ, LT, GT) -> [moira, cai, deutera, hexecosta, ss] |> insertSpace |> Just
        (GT, LT, EQ) -> [moirai, ds, cai, deuteron, hexecostonF] |> insertSpace |> Just
        (GT, LT, GT) -> [moirai, ds, cai, deutera, hexecosta, ss] |> insertSpace |> Just
        (EQ, EQ, LT) -> [moira, cai, proton, hexecostonF] |> insertSpace |> Just
        (EQ, GT, LT) -> [moira, cai, prota, hexecosta, ms] |> insertSpace |> Just
        (GT, EQ, LT) -> [moirai, ds, cai, proton, hexecostonF] |> insertSpace |> Just
        (GT, GT, LT) -> [moirai, ds, cai, prota, hexecosta, ms] |> insertSpace |> Just
        (EQ, EQ, EQ) -> triple
        (EQ, EQ, GT) -> triple
        (EQ, GT, EQ) -> triple
        (EQ, GT, GT) -> triple
        (GT, EQ, EQ) -> triple
        (GT, EQ, GT) -> triple
        (GT, GT, EQ) -> triple
        (GT, GT, GT) -> triple

toPtolemy : SexagesimalTriple -> Maybe (Html msg)
toPtolemy (d, m, s) =
    let (ds, dext) = zeroToNothing d |> Maybe.map Ionian.toMyriads |> Maybe.withDefault (ousia, False) in
    let (ms, mext) = zeroToNothing m |> Maybe.map Ionian.toMyriads |> Maybe.withDefault (ousia, False) in
    let (ss, sext) = zeroToNothing s |> Maybe.map Ionian.toMyriads |> Maybe.withDefault (ousia, False) in
    if dext || mext || sext
    then Nothing
    else [ds, ms, ss] |> insertSpace |> Just

zeroToNothing : BigInt.BigInt -> Maybe BigInt.BigInt
zeroToNothing n =
    case BigInt.compare n zero of
        EQ -> Nothing
        _ -> Just n

insertSpace ns = List.intersperse (text " ") ns |> span []
sp = text " "
cai = text "καὶ"
moira = text "μοῖρα"
moirai = text "μοῖραι"
proton = text "πρῶτον"
prota = text "πρῶτα"
deuteron = text "δεύτερον"
deutera = text "δεύτερα"
hexecoston = text "ἑξηκοστὸν"
hexecosta = text "ἑξηκοστὰ"
hexecostonF = text "ἑξηκοστόν"
hexecostaF = text "ἑξηκοστά"
ousia = text "Ο"
