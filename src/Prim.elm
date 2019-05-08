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
