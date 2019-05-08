module SexagesimalTriple exposing (toCommon, secondsToCommon, secondsToPtolemy)

import Ionian
import BigInt
import Digits exposing (..)
import Dict
import Ionian
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
    let triple =
            if dext || mext || sext
            then Nothing
            else [ds, ms, ss] |> insertSpace |> Just in
    case (BigInt.compare d one, BigInt.compare m one, BigInt.compare s one) of
        (LT, LT, LT) -> [] |> Just
        (LT, LT, EQ) -> [deuteron, hexecoston, ss] |> insertSpace |> Just
        (LT, LT, GT) -> [deutera, hexecosta, ss] |> insertSpace |> Just
        (LT, EQ, LT) -> [proton, hexecoston, ms] |> insertSpace |> Just
        (LT, GT, LT) -> [prota, hexecosta, ms] |> insertSpace |> Just
        (LT, EQ, EQ) -> [proton, hexecoston, ms, cai, deuteron, ss] |> insertSpace |> Just
        (LT, EQ, GT) -> [proton, hexecoston, ms, cai, deutera, ss] |> insertSpace |> Just
        (LT, GT, EQ) -> [prota, hexecosta, ms, cai, deuteron, ss] |> insertSpace |> Just
        (LT, GT, GT) -> [prota, hexecosta, ms, cai, deutera, ss] |> insertSpace |> Just
        (EQ, LT, LT) -> [ds] |> insertSpace |> Just
        (GT, LT, LT) -> [ds] |> insertSpace |> Just
        (EQ, LT, EQ) -> [moira, ds, cai, deuteron, hexecoston, ss] |> insertSpace |> Just
        (EQ, LT, GT) -> [moira, ds, cai, deutera, hexecosta, ss] |> insertSpace |> Just
        (GT, LT, EQ) -> [moirai, ds, cai, deuteron, hexecoston, ss] |> insertSpace |> Just
        (GT, LT, GT) -> [moirai, ds, cai, deutera, hexecosta, ss] |> insertSpace |> Just
        (EQ, EQ, LT) -> [ds, ms] |> insertSpace |> Just
        (EQ, GT, LT) -> [ds, ms] |> insertSpace |> Just
        (GT, EQ, LT) -> [ds, ms] |> insertSpace |> Just
        (GT, GT, LT) -> [ds, ms] |> insertSpace |> Just
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
