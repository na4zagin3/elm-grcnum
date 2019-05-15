module Greek.Fraction exposing (toDiophantus, toHeron)

import BigInt
import Fraction exposing (Frac(..))
import Digits exposing (..)
import Dict
import Greek.Ionian as Ionian
import Prim exposing (..)
import Set

zero = BigInt.fromInt 0
one = BigInt.fromInt 1
two = BigInt.fromInt 2

toDiophantus : Frac -> Maybe (List Element, Bool)
toDiophantus fr =
    case fr of
        SimpleFrac n d ->
            if n == one
            then if d == one
                 then Nothing
                 else if d == two
                      then Just ([Word "𐅵″"], False)
                      else let (ds, dext) = Ionian.toDiophantus False d in
                           Just (ds ++ [Word "″"], dext)
            else let (ns, next) = Ionian.toDiophantus False n in
                 let (ds, dext) = Ionian.toDiophantus False d in
                 Just ([Fraction False ns ds], next || dext)
        Unities f s us ->
            Nothing

toHeron : Frac -> Maybe (List Element, Bool)
toHeron fr =
    let numConv = Ionian.toMyriads False in
    case fr of
        SimpleFrac n d ->
            if n == one
            then if d == one
                 then Nothing
                 else if d == two
                      then Just ([Word "𐅁"], False)
                      else let (ds, dext) = numConv d in
                           Just (ds ++ [Word "″"], dext)
                           |> excludeExtention
            else let (ns, next) = numConv n in
                 let (ds, dext) = numConv d in
                 Just (List.concat [ns, [Word "ʹ"], ds, [Word "″"], ds, [Word "″"]], next || dext)
                 |> excludeExtention
        Unities f s us ->
            let allUnits = [f, s] ++ us |> List.sortWith BigInt.compare in
            if List.map2 BigInt.compare allUnits (List.drop 1 allUnits) |> List.all (\x -> x /= EQ)
            then allUnits
                |> List.map (SimpleFrac one)
                |> List.map toHeron
                |> List.foldl (Maybe.map2 (\(fs, fext) (accs, accext) -> (fs :: accs, fext || accext))) (Just ([], False))
                |> Maybe.map (\(fss, fext) -> (List.reverse fss |> List.concat, fext))
                |> excludeExtention
            else Nothing

excludeExtention : Maybe (a, Bool) -> Maybe (a, Bool)
excludeExtention a = case a of
                         Just (x, False) -> Just (x, False)
                         _ -> Nothing
