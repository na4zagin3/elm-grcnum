module Prim exposing (Element(..), viewElements)

import Html exposing (Html, Attribute, a, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)

type Element = Space
    | WeakBreak
    | NoBreak (List Element) -- ToDo: redesign the model
    | Word String
    | Overline (List (Element))
    -- | Dotted (List (Element))
    -- | Moira
    | Fraction Bool (List (Element)) (List (Element))
    | Myriad (List (Element)) (List (Element))

viewElements : List Element -> List (Html msg)
viewElements es =
    List.map viewElement es

viewElement : Element -> Html msg
viewElement e =
    case e of
        Space -> text " "
        WeakBreak -> wbr [] []
        NoBreak es -> span [style "white-space" "nowrap"] (viewElements es)
        Word w -> text w
        Overline es ->
            span [ style "text-decoration" "overline"
                 , style "line-height" "2em"
                 ] (viewElements es)
        Fraction l d u ->
            let elem i = span
                     [ style "text-align" "center"
                     , style "line-height" "1em"
                     , style "padding" "0 0.1em"
                     ] (viewElements i) in
            let line = span
                     [ style "border-top" "black solid 0.1ex"
                     , style "margin-top" "0.1ex"
                     , style "margin-bottomm" "0.1ex"
                     ] [] in
            span [ style "display" "inline-flex"
                 , style "flex-direction" "column-reverse"
                 , style "vertical-align" "middle"
                 , style "padding" "0 0.1em"
                 ] (if l then [elem d, line, elem u] else [elem d, elem u])
        Myriad m u ->
            span [ style "display" "inline-flex"
                 , style "flex-direction" "column-reverse"
                 ] [ span
                     [ style "text-align" "center"
                     , style "line-height" "1em"
                     ] (viewElements m)
                   , span
                         [ style "text-align" "center"
                         , style "line-height" "1em"
                         , style "font-size" "smaller" ] (viewElements u)
                   ]
