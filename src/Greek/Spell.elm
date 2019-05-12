module Greek.Spell exposing (..)

import Browser
import BigInt
import Digits
import Html exposing (Html, Attribute, a, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Prim exposing (..)

type Number = Singular | Plural
type Case = Nominative | Genitive | Dative | Accusative
type Gender = Masculine | Neuter | Feminine
type Position = Final | Accented | Normal

type alias Adjective = Case -> Gender -> Number -> Position -> String
type alias Adverb = Bool -> String

accented : Position -> Bool
accented p = case p of
                 Final -> True
                 Accented -> True
                 Normal -> False

animate : Gender -> Bool
animate g = case g of
                 Masculine -> True
                 Feminine -> True
                 Neuter -> False

heis : Adjective
heis c g n p =
    let a = accented p in
    case (c, g, n) of
        (Nominative, Masculine, _) -> "εἷς"
        (Nominative, Neuter, _) -> if a then "ἕν" else "ἓν"
        (Genitive, Masculine, _) -> if a then "ἑνός" else "ἑνὸς"
        (Genitive, Neuter, _) -> if a then "ἑνός" else "ἑνὸς"
        (Dative, Masculine, _) -> if a then "ἑνός" else "ἑνὸς"
        (Dative, Neuter, _) -> if a then "ἑνί" else "ἑνὶ"
        (Accusative, Masculine, _) -> "ἕνα"
        (Accusative, Neuter, _) -> if a then "ἕν" else "ἓν"
        (Nominative, Feminine, _) -> "μία"
        (Genitive, Feminine, _) -> "μιᾶς"
        (Dative, Feminine, _) -> "μιᾷ"
        (Accusative, Feminine, _) -> "μίαν"

dyo : Adjective
dyo c g n p =
    case (c, g, p) of
        (Nominative, _, _) -> "δύο"
        (Genitive, _, _) -> "δυῶν"
        (Dative, _, Final) -> "δυσίν"
        (Dative, _, Accented) -> "δυσί"
        (Dative, _, Normal) -> "δυσὶ"
        (Accusative, _, _) -> "δύο"

treis : Adjective
treis c g n p =
    case (c, animate g, p) of
        (Nominative, True, _) -> "τρεῖς"
        (Accusative, True, _) -> "τρεῖς"
        (Nominative, False, _) -> "τρία"
        (Accusative, False, _) -> "τρία"
        (Genitive, _, _) -> "τριῶν"
        (Dative, _, Final) -> "τρισίν"
        (Dative, _, Accented) -> "τρισί"
        (Dative, _, Normal) -> "τρισὶ"

tettares : Adjective
tettares c g n p =
    case (c, animate g, p) of
        (Nominative, True, _) -> "τέτταρες"
        (Accusative, True, _) -> "τέτταρας"
        (Nominative, False, _) -> "τέτταρα"
        (Accusative, False, _) -> "τέτταρα"
        (Genitive, _, _) -> "τεττάρων"
        (Dative, _, Final) -> "τέτταρσιν"
        (Dative, _, Accented) -> "τέτταρσι"
        (Dative, _, Normal) -> "τέτταρσι"

pente : Adjective
pente _ _ _ _ = "πέντε"

hex : Adjective
hex _ _ _ p = if accented p then "ἕξ" else "ἓξ"

hepta : Adjective
hepta _ _ _ p = if accented p then "ἑπτά" else "ἑπτὰ"

octo : Adjective
octo _ _ _ p = if accented p then "ὀκτώ" else "ὀκτὼ"

ennea : Adjective
ennea _ _ _ _ = "ἐννέα"

deca : Adjective
deca _ _ _ _ = "δέκα"

commonCardinal : Case -> Gender -> Int -> Maybe (List Word)
commonCardinal c g n =
    let sg w = w c g Singular |> Word in
    let pl w = w c g Plural |> Word in
    case n of
        1 -> Just [sg heis]
        2 -> Just [sg dyo]
        3 -> Just [sg treis]
        4 -> Just [sg tettares]
        5 -> Just [sg pente]
        6 -> Just [sg hex]
        7 -> Just [sg hepta]
        8 -> Just [sg octo]
        9 -> Just [sg ennea]
        10 -> Just [sg deca]
        _ -> Nothing


type Word = Word (Position -> String) | Enclitic (Position -> String) | Punctation String
renderWords ws = renderWordsSub [] ws

renderWordsSub acc words =
    case words of
        [] -> List.reverse acc |> String.join " "
        [Word w] -> renderWordsSub (w Final::acc) []
        [Enclitic w] -> renderWordsSub (w Final::acc) []
        [Punctation w] -> renderWordsSub (w::acc) []
        w1::w2::w2s ->
           let ws = w2::w2s in
           case (w1, w2) of
               (Word w, Word _) -> renderWordsSub (w Normal::acc) ws
               (Enclitic w, Word _) -> renderWordsSub (w Normal::acc) ws
               (Word w, Enclitic _) -> renderWordsSub (w Accented::acc) ws
               (Enclitic w, Enclitic _) -> renderWordsSub (w Accented::acc) ws
               (Word w, Punctation _) -> renderWordsSub (w Final::acc) ws
               (Enclitic w, Punctation _) -> renderWordsSub (w Final::acc) ws
               (Punctation w, _) -> renderWordsSub (w::acc) ws
