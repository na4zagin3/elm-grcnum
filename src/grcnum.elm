import Browser
import BigInt
import Dict
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Myriads = List (Int)

type alias Model =
  { content : Result String BigInt.BigInt
  }


init : Model
init =
  { content = Err "" }



-- UPDATE


type Msg
  = Change String
  | Increment
  | Decrement

one = BigInt.fromInt 1

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
        case BigInt.fromIntString newContent of
            Nothing ->
                { model | content = Err newContent }
            Just n ->
                { model | content = Ok n }

    Increment ->
        case model.content of
            Ok n ->
                { model | content = BigInt.add n one |> Ok }
            Err _ ->
                model

    Decrement ->
        case model.content of
            Ok n ->
                { model | content = BigInt.sub n one |> Ok }
            Err _ ->
                model



-- VIEW


view : Model -> Html Msg
view model =
    case model.content of
        Err s ->
          div []
            [ input [ placeholder "Number to convert", value s, onInput Change ] []
            , viewNumTable Nothing
            ]
        Ok n ->
          let s = BigInt.toString n in
          div []
            [ input [ placeholder "Number to convert", value s, onInput Change ] []
            , button [ onClick Increment ] [ text "+" ]
            , button [ onClick Decrement ] [ text "-" ]
            , viewNumTable (Just n)
            ]

viewNumTable n =
    let m = Maybe.map explodeIntoMyriads n in
    let orig =
            [ td [] [text "Converted from"]
            , td [ style "word-break" "break-word"]
                [text (Maybe.map BigInt.toString n |> Maybe.withDefault "")]
            ] in
    let label f s =
            let extended = case (n, m) of
                     (Just np, Just mp) -> f np mp
                     (_, _) -> False in
            if extended
            then [text s, text " (extended)"]
            else [text s] in
    let row l c =
            [ td [style "width" "10em"] l
            , td [style "min-height" "2.5em"] c
            ] in
    let common =
            row [text "common"]
                [case (n, m) of
                     (Just np, Just mp) ->
                         if viewNumberMyriadsIsExtended np
                         then text "too big"
                         else viewNumberMyriads mp
                     (_, _) -> text ""
                ] in
    let diophantus =
            row (label viewNumberDiophantusIsExtended "Diophantus")
                [Maybe.map viewNumberDiophantus m |> Maybe.withDefault (text "")] in
    let aristarchus =
            row (label viewNumberAristarchusIsExtended "Aristarchus")
                [Maybe.map viewNumberAristarchus m |> Maybe.withDefault (text "")] in
    let apollonius =
            row (label viewNumberApolloniusIsExtended "Apollonius")
                [Maybe.map viewNumberApollonius m |> Maybe.withDefault (text "")] in
    let body =
            [ tr [] orig
            , tr [] common
            , tr [] diophantus
            , tr [] aristarchus
            , tr [] apollonius
            ] in
    table [style "width" "100%"]
        [ tbody [] body
        ]

viewOverLined : String -> Html msg
viewOverLined s = span
                  [ style "text-decoration" "overline"
                  , style "line-height" "2em"
                  ]
                  [ text s ]

viewMyriad : Html msg -> Html msg -> Html msg
viewMyriad m u =
    span [ style "display" "inline-flex"
         , style "flex-direction" "column-reverse"
         ] [ span [ style "text-align" "center" ] [ m ]
           , span [ style "text-align" "center"
                  , style "font-size" "smaller" ] [ u ]
           ]

viewNumberMyriadsIsExtended : BigInt.BigInt -> Bool
viewNumberMyriadsIsExtended n =
    BigInt.gte n (BigInt.fromInt 100000000)

viewNumberMyriads : Myriads -> Html msg
viewNumberMyriads =
    let f n d =
            let dn = convertSimple d in
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) -> dn |> viewOverLined |> Just
                  (_, _) ->
                      let m = List.repeat n myriadSymbol |> String.join "" |> text in
                      text dn
                      |> viewMyriad m
                      |> Just in
    viewMyriads f (text myriadSeparator)

viewNumberDiophantusIsExtended : BigInt.BigInt -> Myriads -> Bool
viewNumberDiophantusIsExtended n _ =
    BigInt.gte n (BigInt.fromInt 100000000)

viewNumberDiophantus : Myriads -> Html msg
viewNumberDiophantus =
    let f n d = convertSimple d |> viewOverLined |> Just in
    viewMyriads f (span [] [text ".", wbr [] []])

viewNumberAristarchusIsExtended : BigInt.BigInt -> Myriads -> Bool
viewNumberAristarchusIsExtended n _ =
    BigInt.gte n (BigInt.fromInt 100000000)
    || n == (BigInt.fromInt 10000) -- ToDo: Research

viewNumberAristarchus : Myriads -> Html msg
viewNumberAristarchus =
    let f n d = convertSimple d |> viewOverLined |> Just in
    viewMyriads f (span [style "white-space" "nowrap"] [text (myriadSeparator ++ myriadSymbol ++ myriadSeparator), wbr [] []])

viewNumberApolloniusIsExtended : BigInt.BigInt -> Myriads -> Bool
viewNumberApolloniusIsExtended _ m =
    List.length m >= 10000

viewNumberApolloniusSeries : Html msg -> Bool -> Myriads -> Html msg
viewNumberApolloniusSeries sep top =
    let decolateMyriad = if top then viewOverLined else text in
    let f n d =
            case (n, d) of
                  (_, 0) -> Nothing
                  (0, _) ->
                      convertSimple d
                      |> decolateMyriad
                      |> Just
                  (_, _) ->
                      let m =
                              BigInt.fromInt n
                              |> explodeIntoMyriads
                              |> viewNumberApolloniusSeries sep False
                              |> viewMyriad (text myriadSymbol) in
                      span [style "white-space" "nowrap"] [m, text myriadSeparator, convertSimple d |> decolateMyriad]
                      |> Just in
    viewMyriads f sep

viewNumberApollonius : Myriads -> Html msg
viewNumberApollonius = viewNumberApolloniusSeries (text " καὶ ") True

viewNumberApolloniusWithComma : Myriads -> Html msg
viewNumberApolloniusWithComma = viewNumberApolloniusSeries (text ",") True

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

explodeIntoDigits : Int -> List Int
explodeIntoDigits n =
   let sub acc m = if m >= 10 then sub ((modBy 10 m) :: acc) (m // 10) else m::acc
   in
   sub [] n

explodeIntoMyriads : BigInt.BigInt -> Myriads
explodeIntoMyriads n =
    let myriad = BigInt.fromInt 10000 in
    let zero = BigInt.fromInt 0 in
    let bigIntToInt m = BigInt.toString m |> String.toInt |> Maybe.withDefault 0 in
    let sub acc m =
            if BigInt.gt m zero
            then let (q, r) = BigInt.divmod m myriad |> Maybe.withDefault (zero, zero) in
                 let ir = bigIntToInt r in
                 sub (ir :: acc) q
            else acc
    in
    sub [] n

convertSimple : Int -> String
convertSimple n =
    let f sd d = Dict.get d sd |> Maybe.withDefault "" in
    List.reverse (explodeIntoDigits n)
    |> List.map2 f numberSymbols
    |> List.foldl (++) ""
