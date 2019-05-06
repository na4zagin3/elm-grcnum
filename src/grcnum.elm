import Attic
import Browser
import BigInt
import Digits
import Ionian
import Html exposing (Html, Attribute, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import SexagesimalTriple



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
    let orig =
            [ td [] [text "Converted from"]
            , td [ style "word-break" "break-word"]
                [text (Maybe.map BigInt.toString n |> Maybe.withDefault "")]
            ] in
    let row l c =
            [ td [style "width" "10em"] l
            , td [style "min-height" "2.5em"] c
            ] in
    let calcRow l f =
            case n of
                Nothing -> row [text l] [text ""]
                Just np ->
                    let (lext, cs) = f np in
                    row (text l :: lext) cs in
    let ionianRowCommon l f =
            case n of
                Nothing -> row [text l] [text ""]
                Just np ->
                    case f np of
                        (c, False) -> row [text l] [c]
                        (c, True) -> row [text l] [text "too big"] in
    let ionianRow l f =
            case n of
                Nothing -> row [text l] [text ""]
                Just np ->
                    case f np of
                        (c, False) -> row [text l] [c]
                        (c, True) -> row [text l, text " (extended)"] [c] in
    let maybeRow l f =
            case n of
                Nothing -> row [text l] [text ""]
                Just np ->
                    case f np of
                        Nothing -> row [text l] [text "too big"]
                        Just c -> row [text l] [c] in
    let body =
            [ tr [] orig
            , tr [] (maybeRow "Attic" (Attic.toAttic Attic.generalSymbols))
            , tr [] (ionianRowCommon "Common Ionian" Ionian.toMyriads)
            , tr [] (ionianRow "Diophantus" Ionian.toDiophantus)
            , tr [] (ionianRow "Aristarchus" Ionian.toAristarchus)
            , tr [] (ionianRow "Apollonius" Ionian.toApollonius)
            , tr [] (ionianRow "Modified Apollonius" Ionian.toModifiedApollonius)
            , tr [] (calcRow "Sexagesimal triple (in seconds)" (\np -> ([], [viewSexagesimal np])))
            , tr [] (maybeRow "Sexagesimal (EXPERIMENTAL)" (SexagesimalTriple.secondsToCommon))
            , tr [] (maybeRow "Sexagesimal Ptolemy (EXPERIMENTAL)" (SexagesimalTriple.secondsToPtolemy))
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
