module Greek.Sexagesimal exposing (toCommon, secondsToCommon, toPtolemy, secondsToPtolemy)

import BigInt
import Digits exposing (..)
import Dict
import Greek.Ionian as Ionian
import Prim exposing (..)

zero = BigInt.fromInt 0
one = BigInt.fromInt 1

secondsToCommon : BigInt.BigInt -> Maybe (List Element)
secondsToCommon n =
    Digits.explodeIntoSexagesimalTriple n
    |> toCommon

secondsToPtolemy : BigInt.BigInt -> Maybe (List Element)
secondsToPtolemy n =
    Digits.explodeIntoSexagesimalTriple n
    |> toPtolemy

toCommon : SexagesimalTriple -> Maybe (List Element)
toCommon (d, m, s) =
    let (ds, dext) = Ionian.toMyriads d in
    let (ms, mext) = Ionian.toMyriads m in
    let (ss, sext) = Ionian.toMyriads s in
    let concatWords xs =
            if dext || mext || sext
            then Nothing
            else xs |> insertSpace |> Just in
    let triple = [ds, ms, ss] |> concatWords in
    case (BigInt.compare d one, BigInt.compare m one, BigInt.compare s one) of
        (LT, LT, LT) -> [] |> Just
        (LT, LT, EQ) -> [deuteron, hexecoston, ss] |> concatWords
        (LT, LT, GT) -> [deutera, hexecosta, ss] |> concatWords
        (LT, EQ, LT) -> [proton, hexecoston, ms] |> concatWords
        (LT, GT, LT) -> [prota, hexecosta, ms] |> concatWords
        (LT, EQ, EQ) -> [proton, hexecoston, ms, cai, deuteron, ss] |> concatWords
        (LT, EQ, GT) -> [proton, hexecoston, ms, cai, deutera, ss] |> concatWords
        (LT, GT, EQ) -> [prota, hexecosta, ms, cai, deuteron, ss] |> concatWords
        (LT, GT, GT) -> [prota, hexecosta, ms, cai, deutera, ss] |> concatWords
        (EQ, LT, LT) -> [ds] |> concatWords
        (GT, LT, LT) -> [ds] |> concatWords
        (EQ, LT, EQ) -> [moira, ds, cai, deuteron, hexecoston, ss] |> concatWords
        (EQ, LT, GT) -> [moira, ds, cai, deutera, hexecosta, ss] |> concatWords
        (GT, LT, EQ) -> [moirai, ds, cai, deuteron, hexecoston, ss] |> concatWords
        (GT, LT, GT) -> [moirai, ds, cai, deutera, hexecosta, ss] |> concatWords
        (EQ, EQ, LT) -> [ds, ms] |> concatWords
        (EQ, GT, LT) -> [ds, ms] |> concatWords
        (GT, EQ, LT) -> [ds, ms] |> concatWords
        (GT, GT, LT) -> [ds, ms] |> concatWords
        (EQ, EQ, EQ) -> triple
        (EQ, EQ, GT) -> triple
        (EQ, GT, EQ) -> triple
        (EQ, GT, GT) -> triple
        (GT, EQ, EQ) -> triple
        (GT, EQ, GT) -> triple
        (GT, GT, EQ) -> triple
        (GT, GT, GT) -> triple

toPtolemy : SexagesimalTriple -> Maybe (List Element)
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

insertSpace : List (List Element) -> List Element
insertSpace ns = List.intersperse [Space] ns |> List.concat

sp = [Space]
cai = [Word "καὶ"]
moira = [Word "μοῖρα"]
moirai = [Word "μοῖραι"]
proton = [Word "πρῶτον"]
prota = [Word "πρῶτα"]
deuteron = [Word "δεύτερον"]
deutera = [Word "δεύτερα"]
hexecoston = [Word "ἑξηκοστὸν"]
hexecosta = [Word "ἑξηκοστὰ"]
hexecostonF = [Word "ἑξηκοστόν"]
hexecostaF = [Word "ἑξηκοστά"]
ousia = [Word "Ο"]
