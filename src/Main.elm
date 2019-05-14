module Main exposing (main, reactor)

import Attic
import Browser
import BigInt
import Digits
import Ionian
import Html exposing (Html, Attribute, a, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import SexagesimalTriple
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
                , label = "Sexagesimal triple (in seconds)"
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
  , content : Result String BigInt.BigInt
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  ( { translations=flags.translations, content = Err "" }
  , Cmd.none)



-- UPDATE


type Msg
  = Change String
  | Increment
  | Decrement

one = BigInt.fromInt 1

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let noCmd x = (x, Cmd.none) in
  case msg of
    Change newContent ->
        case BigInt.fromIntString newContent of
            Nothing ->
                { model | content = Err newContent } |> noCmd
            Just n ->
                { model | content = Ok n } |> noCmd

    Increment ->
        case model.content of
            Ok n ->
                { model | content = BigInt.add n one |> Ok } |> noCmd
            Err _ ->
                model |> noCmd

    Decrement ->
        case model.content of
            Ok n ->
                { model | content = BigInt.sub n one |> Ok } |> noCmd
            Err _ ->
                model |> noCmd

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    case model.content of
        Err s ->
          div []
            [ input [ placeholder model.translations.numberToConvert, value s, onInput Change ] []
            , viewNumTable model.translations Nothing
            ]
        Ok n ->
          let s = BigInt.toString n in
          div []
            [ input [ placeholder model.translations.numberToConvert, value s, onInput Change ] []
            , button [ onClick Increment ] [ text "+" ]
            , button [ onClick Decrement ] [ text "-" ]
            , viewNumTable model.translations (Just n)
            ]

viewNumTable trn n =
    let orig =
            [ td [] [text trn.convertFrom]
            , td [ style "word-break" "break-word"]
                [text (Maybe.map BigInt.toString n |> Maybe.withDefault "")]
            ] in
    let row l c =
            [ td [style "width" "10em"] l
            , td [style "min-height" "2.5em"] c
            ] in
    let label l =
            case l.href of
                Nothing -> text l.label
                Just h -> a [href h] [text l.label] in
    let calcRow l f =
            case n of
                Nothing -> row [l] [text ""]
                Just np ->
                    let (lext, cs) = f np in
                    row (l :: lext) cs in
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
    let maybeRow l f =
            case n of
                Nothing -> row [l] [text ""]
                Just np ->
                    case f np of
                        Nothing -> row [l] [text trn.tooBig]
                        Just c -> row [l] (viewElements c) in
    let body =
            [ tr [] orig
            , tr [] (maybeRow (label trn.attic) (Attic.toAttic Attic.generalSymbols))
            , tr [] (ionianRowCommon (label trn.commonIonian) Ionian.toMyriads)
            , tr [] (ionianRow (label trn.diophantus) Ionian.toDiophantus)
            , tr [] (ionianRow (label trn.aristarchus) Ionian.toAristarchus)
            , tr [] (ionianRow (label trn.apollonius) Ionian.toApollonius)
            , tr [] (ionianRow (label trn.modifiedApollonius) Ionian.toModifiedApollonius)
            , tr [] (calcRow (label trn.sexagesimalTriple) (\np -> ([], [viewSexagesimal np])))
            , tr [] (maybeRow (label trn.sexagesimal) (SexagesimalTriple.secondsToCommon))
            , tr [] (maybeRow (label trn.sexagesimalPtolemy) (SexagesimalTriple.secondsToPtolemy))
            ] in
    table [style "width" "100%"]
        [ tbody [] body
        ]


zero = BigInt.fromInt 0
sixty = BigInt.fromInt 60

viewSexagesimal : BigInt.BigInt -> Html msg
viewSexagesimal num =
    let (d, m, s) = Digits.explodeIntoSexagesimalTriple num in
    let ds = if BigInt.gt d zero then [BigInt.toString d, "°"] else [] in
    let ms = if BigInt.gt m zero then [BigInt.toString m, "′"] else [] in
    let ss = if BigInt.gt s zero then [BigInt.toString s, "″"] else [] in
    List.concat [ds, ms, ss]
    |> (if BigInt.gte num sixty then (\ x -> x ++ [" = ", BigInt.toString num, "″"]) else identity)
    |> String.join ""
    |> text
