module Main exposing (main, reactor)

import Browser
import BigInt exposing (BigInt)
import Digits exposing (SexagesimalTriple)
import Greek.Attic as Attic
import Greek.Ionian as Ionian
import Greek.Sexagesimal
import Html exposing (Html, Attribute, a, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Sexagesimal exposing (Sexagesimal)
import Prim exposing (..)



-- MAIN


main =
  Browser.element
      { init = init
      , update = update
      , subscriptions = subscriptions
      , view = view
      }

reactor =
  Browser.element
      { init = \ () -> init reactorFlags
      , update = update
      , subscriptions = subscriptions
      , view = view
      }

reactorFlags : Flags
reactorFlags =
    { translations =
          { extended = " (extended)"
          , tooBig = "too big"
          , numberToConvert = "Number to convert"
          , convertFrom = "Converted from"
          , decimalButton = "decimal"
          , decimalFormat = "Input a number without separators. E.g., 1234"
          , sexagesimalButton = "sexagesimal"
          , sexagesimalFormat = "Input in modern sexagesimal notation. E.g., 12;34,5 (meaning 12°34′5″)"
          , attic =
                { href= Just "#attic"
                , label = "Attic"
                }
          , commonIonian =
                { href = Just "#common-ionian"
                , label = "Common Ionian"
                }
          , diophantus =
                { href = Just "#diophantus"
                , label = "Diophantus"
                }
          , aristarchus =
                { href = Just "#aristarchus"
                , label = "Aristarchus"
                }
          , apollonius =
                { href = Just "#apollonius"
                , label = "Apollonius"
                }
          , modifiedApollonius =
                { href = Just "#modified-apollonius"
                , label = "Modified Apollonius"
                }
          , sexagesimalTriple =
                { href = Nothing
                , label = "Sexagesimal triple"
                }
          , sexagesimal =
                { href = Just "#sexagesimal"
                , label = "Sexagesimal (EXPERIMENTAL)"
                }
          , sexagesimalPtolemy =
                { href = Just "#sexagesimal-ptolemy"
                , label = "Sexagesimal Ptolemy (EXPERIMENTAL)"
                }
          }
    }

-- MODEL

type alias Translations =
  { extended: String
  , tooBig: String
  , numberToConvert: String
  , convertFrom: String
  , decimalButton: String
  , decimalFormat: String
  , sexagesimalButton: String
  , sexagesimalFormat: String
  , attic: Label
  , commonIonian: Label
  , diophantus: Label
  , aristarchus: Label
  , apollonius: Label
  , modifiedApollonius: Label
  , sexagesimalTriple: Label
  , sexagesimal: Label
  , sexagesimalPtolemy: Label
  }
type alias Myriads = List (Int)

type alias Label = { href: Maybe String, label: String }
type alias Flags = { translations: Translations }
type alias Model =
  { translations: Translations
  , content : Content
  , input : String
  }

type Content = NumInt (Maybe BigInt) | NumSg (Maybe Sexagesimal)

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { translations=flags.translations, content = NumInt Nothing, input = "" }
  , Cmd.none)



-- UPDATE


type Msg
  = Change String
  | Increment
  | Decrement
  | SelectInt
  | SelectSg

one = BigInt.fromInt 1

updateModelWithInput model newContent =
  case model.content of
      NumInt _ ->
          case BigInt.fromIntString newContent of
              Just n ->
                  { model | content = Just n |> NumInt }
              Nothing ->
                  { model | content = Nothing |> NumInt}
      NumSg _ ->
          case Sexagesimal.fromString newContent of
              Just n ->
                  { model | content = Just n |> NumSg }
              Nothing ->
                  { model | content = Nothing |> NumSg }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let noCmd x = (x, Cmd.none) in
  case msg of
    Change newContent ->
        updateModelWithInput { model | input = newContent } newContent |> noCmd

    Increment ->
        case model.content of
            NumInt (Just n) ->
                let newNum = BigInt.add n one in
                { model | content = newNum |> Just |> NumInt, input = BigInt.toString newNum } |> noCmd
            _ ->
                model |> noCmd

    Decrement ->
        case model.content of
            NumInt (Just n) ->
                let newNum = BigInt.sub n one in
                { model | content = newNum |> Just |> NumInt, input = BigInt.toString newNum } |> noCmd
            _ ->
                model |> noCmd

    SelectInt ->
        updateModelWithInput { model | content = Nothing |> NumInt } model.input |> noCmd
    SelectSg ->
        updateModelWithInput { model | content = Nothing |> NumSg } model.input |> noCmd

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    let trn = model.translations in
    let intBtn = button [ onClick SelectInt ] [ text trn.decimalButton ] in
    let sgBtn = button [ onClick SelectSg ] [ text trn.sexagesimalButton ] in
    case model.content of
        NumInt Nothing ->
          div []
            [ intBtn
            , sgBtn
            , input [ placeholder model.translations.numberToConvert, value model.input, onInput Change ] []
            , div [] [ text trn.decimalFormat ]
            , viewNumTable model.translations Nothing
            ]
        NumInt (Just n) ->
          div []
            [ intBtn
            , sgBtn
            , input [ placeholder model.translations.numberToConvert, value model.input, onInput Change ] []
            , button [ onClick Increment ] [ text "+" ]
            , button [ onClick Decrement ] [ text "-" ]
            , div [] [ text trn.decimalFormat ]
            , viewNumTable model.translations (Just n)
            ]
        NumSg Nothing ->
          div []
            [ intBtn
            , sgBtn
            , input [ placeholder model.translations.numberToConvert, value model.input, onInput Change ] []
            , div [] [ text trn.sexagesimalFormat ]
            , viewSgTable model.translations Nothing
            ]
        NumSg (Just n) ->
          div []
            [ intBtn
            , sgBtn
            , input [ placeholder model.translations.numberToConvert, value model.input, onInput Change ] []
            -- , button [ onClick Increment ] [ text "+" ]
            -- , button [ onClick Decrement ] [ text "-" ]
            , div [] [ text trn.sexagesimalFormat ]
            , viewSgTable model.translations (Just n)
            ]

row l c =
    [ td [style "width" "10em"] l
    , td [style "min-height" "2.5em"] c
    ]
label l =
    case l.href of
        Nothing -> text l.label
        Just h -> a [href h] [text l.label]
origRow trn s =
    [ td [] [text trn.convertFrom]
    , td [ style "word-break" "break-word"]
        [text s]
    ]

calcRow n l f =
    case n of
        Nothing -> row [l] [text ""]
        Just np ->
            let (lext, cs) = f np in
            row (l :: lext) cs

maybeRow trn n l f =
    case n of
        Nothing -> row [l] [text ""]
        Just np ->
            case f np of
                Nothing -> row [l] [text trn.tooBig]
                Just c -> row [l] (viewElements c)

viewNumTable trn n =
    let ionianRowCommon l f =
            case n of
                Nothing -> row [l] [text ""]
                Just np ->
                    case f np of
                        (c, False) -> row [l] (viewElements c)
                        (c, True) -> row [l] [text trn.tooBig] in
    let ionianRow l f =
            case n of
                Nothing -> row [l] [text ""]
                Just np ->
                    case f np of
                        (c, False) -> row [l] (viewElements c)
                        (c, True) -> row [l, text trn.extended] (viewElements c) in
    let body =
            [ tr [] (origRow trn (Maybe.map BigInt.toString n |> Maybe.withDefault ""))
            , tr [] (maybeRow trn n (label trn.attic) (Attic.toAttic Attic.generalSymbols))
            , tr [] (ionianRowCommon (label trn.commonIonian) Ionian.toMyriads)
            , tr [] (ionianRow (label trn.diophantus) Ionian.toDiophantus)
            , tr [] (ionianRow (label trn.aristarchus) Ionian.toAristarchus)
            , tr [] (ionianRow (label trn.apollonius) Ionian.toApollonius)
            , tr [] (ionianRow (label trn.modifiedApollonius) Ionian.toModifiedApollonius)
            ] in
    table [style "width" "100%"]
        [ tbody [] body
        ]

viewSgTable trn n =
    let st = Maybe.andThen sexagesimalToTriple n in
    let body =
            [ tr [] (origRow trn (Maybe.map Sexagesimal.toString n |> Maybe.withDefault ""))
            , tr [] (calcRow st (label trn.sexagesimalTriple) (\np -> ([], [viewSexagesimal np])))
            , tr [] (maybeRow trn st (label trn.sexagesimal) (Greek.Sexagesimal.toCommon))
            , tr [] (maybeRow trn st (label trn.sexagesimalPtolemy) (Greek.Sexagesimal.toPtolemy))
            ] in
    table [style "width" "100%"]
        [ tbody [] body
        ]

zero = BigInt.fromInt 0
sixty = BigInt.fromInt 60

sexagesimalToTriple : Sexagesimal -> Maybe SexagesimalTriple
sexagesimalToTriple sgr =
    let sg = Sexagesimal.trimZeros sgr in
    let i = sg.integers |> Sexagesimal.listToBigInt in
    case sg.fractions of
        [] -> Just (i, zero, zero)
        [a] -> Just (i, a, zero)
        [a, b] -> Just (i, a, b)
        _ -> Nothing


viewSexagesimal : SexagesimalTriple -> Html msg
viewSexagesimal (d, m, s) =
    let ds = if BigInt.gt d zero then [BigInt.toString d, "°"] else [] in
    let ms = if BigInt.gt m zero then [BigInt.toString m, "′"] else [] in
    let ss = if BigInt.gt s zero then [BigInt.toString s, "″"] else [] in
    let num = BigInt.mul d sixty |> BigInt.add m |> BigInt.mul sixty |> BigInt.add s in
    List.concat [ds, ms, ss]
    |> (if BigInt.gte num sixty then (\ x -> x ++ [" = ", BigInt.toString num, "″"]) else identity)
    |> String.join ""
    |> text
