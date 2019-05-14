module Greek.Ionian exposing (toMyriads,
                            toDiophantus,
                            toAristarchus,
                            toApollonius,
                            toModifiedApollonius
                       )

import BigInt
import Digits exposing (..)
import Dict
import Prim exposing (..)


toMyriads : Bool -> BigInt.BigInt -> (List Element, Bool)
toMyriads ol num =
    let decolate m = if ol then [Overline m] else m in
    let m = explodeIntoMyriads num in
    let f n d =
            let dn = convertSimple d in
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) -> decolate [Word dn] |> Just
                  (_, _) ->
                      let ms = List.repeat n myriadSymbol |> String.join "" |> Word in
                      [Myriad [ms] [Word dn]]
                      |> Just in
    ( viewMyriads f [Word myriadSeparator] m
    , BigInt.gte num (BigInt.fromInt 100000000))

toDiophantus : Bool -> BigInt.BigInt -> (List Element, Bool)
toDiophantus ol num =
    let decolate m = if ol then [Overline m] else m in
    let m = explodeIntoMyriads num in
    let f n d = decolate [convertSimple d |> Word] |> Just in
    ( viewMyriads f [Word ".", WeakBreak] m
    , BigInt.gte num (BigInt.fromInt 100000000))

toAristarchus : Bool -> BigInt.BigInt -> (List Element, Bool)
toAristarchus ol num =
    let decolate m = if ol then [Overline m] else m in
    let m = explodeIntoMyriads num in
    let f n d = [Overline [convertSimple d |> Word]] |> Just in
    let sep2 = [NoBreak [Word (myriadSeparator ++ myriadSymbol ++ myriadSeparator)], WeakBreak] in
    (viewMyriads f sep2 m,
    BigInt.gte num (BigInt.fromInt 100000000)
    || num == (BigInt.fromInt 10000) -- ToDo: Research
    )

toApolloniusIsExtended : Myriads -> Bool
toApolloniusIsExtended m =
    List.length m >= 10000

toApolloniusSeries : List Element -> List Element -> List Element -> Bool -> Myriads -> List Element
toApolloniusSeries sep mSymb monad top myriads =
    let decolateMyriad m = if top then [Overline m] else m in
    let f n d =
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) ->
                      let c = [convertSimple d |> Word] |> decolateMyriad in
                      (if List.length myriads <= 1
                        then c
                        else monad ++ c)
                      |> NoBreak
                      |> List.singleton
                      |> Just
                  (_, _) ->
                      let m = BigInt.fromInt n
                              |> explodeIntoMyriads
                              |> toApolloniusSeries sep mSymb monad False
                              |> Myriad mSymb in
                      [m, Word myriadSeparator] ++ ([convertSimple d |> Word] |> decolateMyriad)
                      |> NoBreak
                      |> List.singleton
                      |> Just in
    viewMyriads f sep myriads

toApollonius : Bool -> BigInt.BigInt -> (List Element, Bool)
toApollonius ol num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries [Space, Word "καὶ", Space] [Word smallMyriadSymbol] [Myriad [Word smallMyriadSymbol] [Word "ο"], Word myriadSeparator] ol m
    , toApolloniusIsExtended m
    )

toModifiedApollonius : Bool -> BigInt.BigInt -> (List Element, Bool)
toModifiedApollonius ol num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries [Space, Word "καὶ", Space] [Word myriadSymbol] [Myriad [Word smallMyriadSymbol] [Word "ο"], Word myriadSeparator] ol m
    , toApolloniusIsExtended m
    )

toApolloniusWithComma : Bool -> BigInt.BigInt -> (List Element, Bool)
toApolloniusWithComma ol num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries [Word ","] [Word smallMyriadSymbol] [] ol m
    , toApolloniusIsExtended m
    )

viewMyriads : (Int -> Int -> Maybe (List Element)) -> List Element -> Myriads -> List Element
viewMyriads f sep ms =
    List.reverse ms
    |> List.indexedMap f
    |> List.reverse
    |> List.filterMap identity
    |> List.intersperse sep
    |> List.concat

-- Converter
myriadSymbol = "Μ"
smallMyriadSymbol = "μ"
myriadSeparator = "\u{2009}"
keraia = "ʹ"
gnls = "͵"
ones = ["", "α", "β", "γ", "δ", "ε", "ϛ", "ζ", "η", "θ"]
tens = ["", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "ϟ"]
hundreds = ["", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω", "ϡ"]
applyNonEmpty f s = case s of
   "" -> s
   _ -> f s

thousands = List.map (applyNonEmpty (\ s -> gnls ++ s)) ones

numberSymbols : List (Dict.Dict Int String)
numberSymbols = (List.map (\ xs -> List.indexedMap Tuple.pair xs |> Dict.fromList) [ones, tens, hundreds, thousands])

convertSimple : Int -> String
convertSimple n =
    let f sd d = Dict.get d sd |> Maybe.withDefault "" in
    List.reverse (explodeIntoDigits n)
    |> List.map2 f numberSymbols
    |> List.foldl (++) ""
