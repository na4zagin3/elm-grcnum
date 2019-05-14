module Fraction exposing (..)

import Ionian
import BigInt
import BigInt exposing (BigInt)
import Digits exposing (..)
import Dict
import Ionian
import Prim exposing (..)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, int)
import Set


type Frac = SimpleFrac BigInt BigInt | Unities BigInt BigInt (List BigInt)
type alias MixedFrac = ( BigInt, Frac )


fromString : String -> Maybe Frac
fromString s =
    case Parser.run (succeed identity |= parseFrac |. Parser.end) s of
        Ok n -> Just n
        Err _ -> Nothing

toString : Frac -> String
toString fr =
    case fr of
        SimpleFrac n d ->
            [n, d] |> List.map BigInt.toString |> String.join "/"
        Unities f s us ->
            [f, s] ++ us
            |> List.map (\u -> "1/" ++ BigInt.toString u)
            |> String.join "+"

mixedFromString : String -> Maybe MixedFrac
mixedFromString s =
    case Parser.run (succeed identity |= parseMixedFrac |. Parser.end) s of
        Ok n -> Just n
        Err _ -> Nothing

mixedToString : MixedFrac -> String
mixedToString (i, f) =
    BigInt.toString i ++ toString f

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

parseSimpleFrac : Parser Frac
parseSimpleFrac =
    succeed SimpleFrac
        |= parseDigitsBigInt
        |. Parser.symbol "/"
        |= parseDigitsBigInt

parseUnit : Parser BigInt
parseUnit =
    succeed identity
        |. Parser.symbol "1/"
        |= parseDigitsBigInt

parseUnities : Parser Frac
parseUnities =
    succeed Unities
      |= parseUnit
      |. Parser.symbol "+"
      |= parseUnit
      |= Parser.sequence
          { start = ""
          , separator = ""
          , end = ""
          , spaces = succeed ()
          , item = succeed identity |. Parser.symbol "+" |= parseUnit
          , trailing = Parser.Forbidden
          }

parseFrac : Parser Frac
parseFrac =
    Parser.oneOf [ parseUnities |> Parser.backtrackable
                 , parseSimpleFrac]

parseMixedFrac : Parser MixedFrac
parseMixedFrac =
    succeed (\n d -> (n, d))
        |= parseDigitsBigInt
        |= parseFrac

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
