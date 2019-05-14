module Sexagesimal exposing (..)

import BigInt
import BigInt exposing (BigInt)
import Digits exposing (..)
import Dict
import Prim exposing (..)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int)
import Set


type alias Sexagesimal = { integers: List BigInt, fractions: List BigInt }

integers s = s.integers
fractions s = s.fractions

bi0 = BigInt.fromInt 0
bi60 = BigInt.fromInt 60
listToBigInt : List BigInt -> BigInt
listToBigInt = List.foldr (\ x a -> BigInt.add (BigInt.mul a bi60) x) (BigInt.fromInt 0)

toBigInt : Sexagesimal -> Maybe BigInt
toBigInt s =
    let f x a =
            case BigInt.divmod (BigInt.add x a) bi60 of
                Nothing -> Nothing
                Just (q, r) -> if r == bi0
                          then Just q
                          else Nothing in
    let fr = List.foldl (\x -> Maybe.andThen (f x)) (Just bi0) s.fractions in
    fr |> Maybe.map (\r -> s.integers |> listToBigInt |> BigInt.add r)

toBigIntWithCoeff : Sexagesimal -> (BigInt, Int)
toBigIntWithCoeff s =
    let sub (n, c) =
            if c == 0
            then (n, c)
            else case BigInt.divmod n bi60 of
                     Nothing -> (n, c)
                     Just (q, r) -> if r == bi0
                                    then sub (q, c - 1)
                                    else (n, c) in
    let sn = trimZeros s in
    let c = List.length sn.fractions in
    sub ((shiftDigits c sn).integers |> listToBigInt, c)

fromBigInt : BigInt -> Sexagesimal
fromBigInt b =
    let is = Digits.explodeBigIntBy bi60 b |> List.reverse in
    Sexagesimal is [] |> trimZeros

fromBigIntWithCoeff : (BigInt, Int) -> Sexagesimal
fromBigIntWithCoeff (b, c) =
    (shiftDigits (-c) (fromBigInt b)) |> trimZeros

takeWithDefault x n xs = List.take n (xs ++ List.repeat n x)
takeWithDefaultZero = takeWithDefault (BigInt.fromInt 0)

shiftDigits : Int -> Sexagesimal -> Sexagesimal
shiftDigits n s =
    case compare n 0 of
        LT ->
            let m = -n in
            let xs = takeWithDefaultZero m s.integers |> List.reverse in
            { integers = List.drop m s.integers
            , fractions = xs ++ s.fractions
            }
        GT ->
            let xs = takeWithDefaultZero n s.fractions |> List.reverse in
            { fractions = List.drop n s.fractions
            , integers = xs ++ s.integers
            }
        EQ -> s

normalize : Sexagesimal -> Sexagesimal
normalize x = toBigIntWithCoeff x |> fromBigIntWithCoeff

dropWhile : (x -> Bool) -> List x -> List x
dropWhile pred xs =
    case xs of
        [] -> xs
        (x::xt) -> if pred x
                   then dropWhile pred xt
                   else xs

trimZeros : Sexagesimal -> Sexagesimal
trimZeros s =
    let f xs = xs |> List.reverse |> dropWhile (\x -> BigInt.fromInt 0 == x) |> List.reverse in
    { integers = f s.integers
    , fractions = f s.fractions
    }

fromString : String -> Maybe Sexagesimal
fromString s =
    case Parser.run (succeed identity |= parseSexagesimal |. Parser.end) s of
        Ok n -> Just n
        Err _ -> Nothing

toString : Sexagesimal -> String
toString s =
    let is = List.reverse s.integers |> List.map BigInt.toString |> String.join "," in
    let fs = s.fractions |> List.map BigInt.toString |> String.join "," in
    case (String.isEmpty is, String.isEmpty fs) of
        (True, True) -> "0"
        (False, True) -> is
        (True, False) -> "0;" ++ fs
        (False, False) -> is ++ ";" ++ fs

parseDigits : Parser String
parseDigits =
    Parser.variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.fromList []
        }

parseDigitsBigInt : Parser BigInt
parseDigitsBigInt =
    Parser.andThen (\n -> BigInt.fromIntString n |> maybeToParser "Cannot parse as digits") parseDigits

parseSexagesimalDigits : Parser (List BigInt)
parseSexagesimalDigits =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = succeed ()
        , item = parseDigitsBigInt
        , trailing = Parser.Forbidden
        }

parseSexagesimal =
    succeed Sexagesimal
        |= Parser.map List.reverse parseSexagesimalDigits
        |= Parser.oneOf
           [ succeed identity |. Parser.symbol ";" |= parseSexagesimalDigits
           , succeed []
           ]

maybeToParser : String -> Maybe a -> Parser a
maybeToParser reason x =
    case x of
        Nothing -> Parser.problem reason
        Just y -> Parser.succeed y

parseMapMaybe : String -> Parser (Maybe a) -> Parser a
parseMapMaybe reason p =
    let f x = case x of
                  Nothing -> Parser.problem reason
                  Just y -> Parser.succeed y in
    Parser.andThen f p
