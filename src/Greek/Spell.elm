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
type alias Adverb = Position -> String

accented : Position -> Bool
accented p = case p of
                 Final -> True
                 Accented -> True
                 Normal -> False

enclined : Position -> Bool
enclined p = case p of
                 Final -> False
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

hendeca : Adjective
hendeca _ _ _ p = if enclined p then "ἕνδεκά" else "ἕνδεκα"

dodeca : Adjective
dodeca _ _ _ p = if enclined p then "δώδεκά" else "δώδεκα"

eicosin : Adjective
eicosin _ _ _ p = case p of
                     Final -> "εἴκοσιν"
                     Accented -> "εἴκοσιί"
                     Normal -> "εἴκοσι"

triaconta : Adjective
triaconta _ _ _ p = conta "τριά" p
tettaraconta : Adjective
tettaraconta _ _ _ p = conta "τετταρά" p
penteconta : Adjective
penteconta _ _ _ p = conta "πεντή" p
hexeconta : Adjective
hexeconta _ _ _ p = conta "ἑξή" p
hebdomeconta : Adjective
hebdomeconta _ _ _ p = conta "ἑβδομή" p
ogdoeconta : Adjective
ogdoeconta _ _ _ p = conta "ὀγδοή" p
eneneconta : Adjective
eneneconta _ _ _ p = conta "ἐνενή" p

hecaton : Adjective
hecaton _ _ _ p = if accented p then "ἑκατόν" else "ἑκατὸν"
diacosioi : Adjective
diacosioi = cosioi diaP
triacosioi : Adjective
triacosioi = cosioi triaP
tetracosioi : Adjective
tetracosioi = cosioi tetraP
pentacosioi : Adjective
pentacosioi = cosioi pentaP
hexacosioi : Adjective
hexacosioi = cosioi hexaP
heptacosioi : Adjective
heptacosioi = cosioi heptaP
octacosioi : Adjective
octacosioi = cosioi octaP
enacosioi : Adjective
enacosioi = cosioi enaP

conta : String -> Position -> String
conta s p =
    case p of
        Final -> s ++ "κοντα"
        Accented -> s ++ "κοντά"
        Normal -> s ++ "κοντα"

cosioi : String -> Adjective
cosioi s = vowelStemIos (s ++ "κόσ") (s ++ "κοσ")

-- ToDo: Support enclitics
vowelStemIos : String -> String -> Adjective
vowelStemIos ss ls c g n p =
    case (c, g, n) of
        (Nominative, Masculine, Singular) -> ss ++ "ιος"
        (Accusative, Masculine, Singular) -> ss ++ "ιον"
        (Nominative, Neuter, Singular) -> ss ++ "ιον"
        (Accusative, Neuter, Singular) -> ss ++ "ιον"
        (Genitive, Masculine, Singular) -> ls ++ "ίου"
        (Genitive, Neuter, Singular) -> ls ++ "ίου"
        (Dative, Masculine, Singular) -> ls ++ "ίῳ"
        (Dative, Neuter, Singular) ->  ls ++ "ίῳ"

        (Nominative, Feminine, Singular) -> ls ++ "ία"
        (Accusative, Feminine, Singular) -> ls ++ "ίαν"
        (Genitive, Feminine, Singular) -> ls ++ "ίας"
        (Dative, Feminine, Singular) ->  ls ++ "ίᾳ"

        (Nominative, Masculine, Plural) -> ss ++ "ιοι"
        (Accusative, Masculine, Plural) -> ls ++ "ίους"
        (Nominative, Neuter, Plural) -> ss ++ "ια"
        (Accusative, Neuter, Plural) -> ss ++ "ια"
        (Genitive, Masculine, Plural) -> ls ++ "ίων"
        (Genitive, Neuter, Plural) -> ls ++ "ίων"
        (Dative, Masculine, Plural) -> ls ++ "ίοις"
        (Dative, Neuter, Plural) ->  ls ++ "ίοις"

        (Nominative, Feminine, Plural) -> ss ++ "ιαι"
        (Accusative, Feminine, Plural) -> ls ++ "ίας"
        (Genitive, Feminine, Plural) -> ls ++ "ίων"
        (Dative, Feminine, Plural) ->  ls ++ "ίαις"

cai : Word
cai =
    Word (\p -> case p of
                    Final -> "καί"
                    Accented -> "καί"
                    Normal -> "καὶ")

caideca : String -> Word
caideca s = Word (\p ->
    case p of
        Final -> s ++ "καίδεκα"
        Accented -> s ++ "καίδεκά"
        Normal -> s ++ "καίδεκα" )


diaP : String
diaP = "δια"
triaP : String
triaP = "τρια"
tetraP = "τετρα"
pentaP = "πεντα"
hexaP = "ἑξα"
octaP = "ὀκτα"
enaP = "ἐνα"

treisP : String
treisP = "τρεισ"
tettaresP : String
tettaresP = "τετταρεσ"
penteP : String
penteP = "πεντε"
hecP : String
hecP = "ἑκ"
heptaP : String
heptaP = "ἑπτα"
octoP : String
octoP = "ὀκτω"
enneaP : String
enneaP = "ἐννεα"

commonCardinalFromDigits : Case -> Gender -> List Int -> Maybe (List Word)
commonCardinalFromDigits c g n =
    let sg w = w c g Singular |> Word in
    let pl w = w c g Plural |> Word in
    let reorder2 tx ty =
            List.filterMap identity [ty, tx]
            |> List.intersperse [cai]
            |> List.concat
    in
    let reorder3 tx ty tz =
            List.filterMap identity [tz, ty, tx]
            |> List.intersperse [cai]
            |> List.concat
    in
    let digit zeros x =
            if x == 0
            then Just Nothing
            else
              x :: (List.repeat zeros 0)
              |> commonCardinalFromDigits c g
              |> Maybe.map Just
    in
    case n of
        [1] -> Just [sg heis]
        [2] -> Just [pl dyo]
        [3] -> Just [pl treis]
        [4] -> Just [pl tettares]
        [5] -> Just [pl pente]
        [6] -> Just [pl hex]
        [7] -> Just [pl hepta]
        [8] -> Just [pl octo]
        [9] -> Just [pl ennea]
        [1,0] -> Just [pl deca]
        [1,1] -> Just [pl hendeca]
        [1,2] -> Just [pl dodeca]
        [1,3] -> Just [pl treis, cai, pl deca]
        [1,4] -> Just [pl tettares, cai, pl deca]
        [1,5] -> Just [caideca penteP]
        [1,6] -> Just [caideca hecP]
        [1,7] -> Just [caideca heptaP]
        [1,8] -> Just [caideca octoP]
        [1,9] -> Just [caideca enneaP]
        [2,0] -> Just [pl eicosin]
        [3,0] -> Just [pl triaconta]
        [4,0] -> Just [pl tettaraconta]
        [5,0] -> Just [pl penteconta]
        [6,0] -> Just [pl hexeconta]
        [7,0] -> Just [pl hebdomeconta]
        [8,0] -> Just [pl ogdoeconta]
        [9,0] -> Just [pl eneneconta]
        [x,y] -> Maybe.map2 reorder2
                 (digit 1 x)
                 (digit 0 y)
        [1,0,0] -> Just [pl hecaton]
        [2,0,0] -> Just [pl diacosioi]
        [3,0,0] -> Just [pl triacosioi]
        [4,0,0] -> Just [pl tetracosioi]
        [5,0,0] -> Just [pl pentacosioi]
        [6,0,0] -> Just [pl hexacosioi]
        [7,0,0] -> Just [pl heptacosioi]
        [8,0,0] -> Just [pl octacosioi]
        [9,0,0] -> Just [pl enacosioi]
        [x,y,z] -> Maybe.map3 reorder3
                 (digit 2 x)
                 (digit 1 y)
                 (digit 0 z)
        _ -> Nothing

commonCardinal : Case -> Gender -> Int -> Maybe (List Word)
commonCardinal c g n =
    Digits.explodeIntoDigits n
    |> commonCardinalFromDigits c g


type Word = Word (Position -> String) | Enclitic (Position -> String) | Punctation String
renderWords ws = renderWordsSub [] ws

renderWordsSub acc words =
    case words of
        [] -> List.reverse acc |> String.join " "
        [Word w] -> renderWordsSub (w Final::acc) []
        -- ToDo: Support two-syllable enclitics
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
