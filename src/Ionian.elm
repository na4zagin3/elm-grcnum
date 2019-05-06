module Ionian exposing (toMyriads,
                            toDiophantus,
                            toAristarchus,
                            toApollonius,
                            toModifiedApollonius
                       )

import BigInt
import Digits exposing (..)
import Dict
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)


overLined : String -> Html msg
overLined s = span
                  [ style "text-decoration" "overline"
                  , style "line-height" "2em"
                  ]
                  [ text s ]

stackMyriad : Html msg -> Html msg -> Html msg
stackMyriad m u =
    span [ style "display" "inline-flex"
         , style "flex-direction" "column-reverse"
         ] [ span
             [ style "text-align" "center"
             , style "line-height" "1em"
             ] [ m ]
           , span
                 [ style "text-align" "center"
                 , style "line-height" "1em"
                 , style "font-size" "smaller" ] [ u ]
           ]

toMyriads : BigInt.BigInt -> (Html msg, Bool)
toMyriads num =
    let m = explodeIntoMyriads num in
    let f n d =
            let dn = convertSimple d in
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) -> dn |> overLined |> Just
                  (_, _) ->
                      let ms = List.repeat n myriadSymbol |> String.join "" |> text in
                      text dn
                      |> stackMyriad ms
                      |> Just in
    ( viewMyriads f (text myriadSeparator) m
    , BigInt.gte num (BigInt.fromInt 100000000))

toDiophantus : BigInt.BigInt -> (Html msg, Bool)
toDiophantus num =
    let m = explodeIntoMyriads num in
    let f n d = convertSimple d |> overLined |> Just in
    ( viewMyriads f (span [] [text ".", wbr [] []]) m
    , BigInt.gte num (BigInt.fromInt 100000000))

toAristarchus : BigInt.BigInt -> (Html msg, Bool)
toAristarchus num =
    let m = explodeIntoMyriads num in
    let f n d = convertSimple d |> overLined |> Just in
    let sep = span [style "white-space" "nowrap"] [text (myriadSeparator ++ myriadSymbol ++ myriadSeparator), wbr [] []] in
    (viewMyriads f sep m,
    BigInt.gte num (BigInt.fromInt 100000000)
    || num == (BigInt.fromInt 10000) -- ToDo: Research
    )

toApolloniusIsExtended : Myriads -> Bool
toApolloniusIsExtended m =
    List.length m >= 10000

toApolloniusSeries : Html msg -> Html msg -> List (Html msg) -> Bool -> Myriads -> Html msg
toApolloniusSeries sep mSymb monad top myriads =
    let decolateMyriad = if top then overLined else text in
    let f n d =
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) ->
                      let c = convertSimple d |> decolateMyriad in
                      span
                        [style "white-space" "nowrap"]
                        (if List.length myriads <= 1
                         then [c]
                         else monad ++ [c])
                      |> Just
                  (_, _) ->
                      let m = BigInt.fromInt n
                              |> explodeIntoMyriads
                              |> toApolloniusSeries sep mSymb monad False
                              |> stackMyriad mSymb in
                      span [style "white-space" "nowrap"] [m, text myriadSeparator, convertSimple d |> decolateMyriad]
                      |> Just in
    viewMyriads f sep myriads

toApollonius : BigInt.BigInt -> (Html msg, Bool)
toApollonius num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries (text " καὶ ") (text smallMyriadSymbol) [stackMyriad (text smallMyriadSymbol) (text "ο"), text myriadSeparator] True m
    , toApolloniusIsExtended m
    )

toModifiedApollonius : BigInt.BigInt -> (Html msg, Bool)
toModifiedApollonius num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries (text " καὶ ") (text myriadSymbol) [stackMyriad (text smallMyriadSymbol) (text "ο"), text myriadSeparator] True m
    , toApolloniusIsExtended m
    )

toApolloniusWithComma : BigInt.BigInt -> (Html msg, Bool)
toApolloniusWithComma num =
    let m = explodeIntoMyriads num in
    (toApolloniusSeries (text ",") (text smallMyriadSymbol) [] True m
    , toApolloniusIsExtended m
    )

viewMyriads : (Int -> Int -> Maybe (Html msg)) -> Html msg -> Myriads -> Html msg
viewMyriads f sep ms =
    List.reverse ms
    |> List.indexedMap f
    |> List.reverse
    |> List.filterMap identity
    |> List.intersperse sep
    |> span []

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
