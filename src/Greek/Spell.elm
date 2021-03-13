module Greek.Spell exposing (..)

import Browser
import BigInt
import Digits
import Html exposing (Html, Attribute, a, button, div, input, table, tbody, td, tr, text, span, wbr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Inflection exposing (Number(..), Case(..), Gender(..))
import Maybe.Extra
import Prim exposing (..)
import Regex

type CardinalOrder =
      Ascending
    | Descending
    | DescendingJuxtapose

type Position = Final | Accented | Normal

type alias Noun = Case -> Number -> Position -> String
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

movableNu : String
movableNu = "(ν)"

adP : String -> Noun
adP pf c n p =
    let a = accented p in
    pf ++ case (c, n) of
        (Nominative, Singular) -> if a then "άς" else "ὰς"
        (Nominative, Plural) -> "άδες"
        (Genitive, Singular) -> "άδος"
        (Genitive, Plural) -> "άδων"
        (Dative, Singular) -> "άδι"
        (Dative, Plural) -> "άδσι" ++ movableNu
        (Accusative, Singular) -> "άδα"
        (Accusative, Plural) -> "άδες"

monad : Noun
monad = adP "μον"

myriad : Noun
myriad = adP "μυρι"

plousP : String -> Adjective
plousP pf c g n _ =
    pf ++ case (c, g, n) of
        (Nominative, Masculine, Singular) -> "πλοῦς"
        (Accusative, Masculine, Singular) -> "πλοῦν"
        (Nominative, Neuter, Singular) -> "πλοῦν"
        (Accusative, Neuter, Singular) -> "πλοῦν"
        (Genitive, Masculine, Singular) -> "πλοῦ"
        (Genitive, Neuter, Singular) -> "πλοῦ"
        (Dative, Masculine, Singular) -> "πλῷ"
        (Dative, Neuter, Singular) -> "πλῷ"
        (Nominative, Feminine, Singular) -> "πλῆ"
        (Accusative, Feminine, Singular) -> "πλῆν"
        (Genitive, Feminine, Singular) -> "πλῆς"
        (Dative, Feminine, Singular) -> "πλῇ"

        (Nominative, Masculine, Plural) -> "πλοῖ"
        (Accusative, Masculine, Plural) -> "πλοῦς"
        (Nominative, Neuter, Plural) -> "πλᾶ"
        (Accusative, Neuter, Plural) -> "πλᾶ"
        (Genitive, Masculine, Plural) -> "πλῶν"
        (Genitive, Neuter, Plural) -> "πλῶν"
        (Dative, Masculine, Plural) -> "πλοῖς"
        (Dative, Neuter, Plural) -> "πλοῖς"
        (Nominative, Feminine, Plural) -> "πλαῖ"
        (Accusative, Feminine, Plural) -> "πλᾶς"
        (Genitive, Feminine, Plural) -> "πλῶν"
        (Dative, Feminine, Plural) -> "πλαῖς"

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
        (Dative, _, Final) -> "τρισί" ++ movableNu
        (Dative, _, Accented) -> "τρισί" ++ movableNu
        (Dative, _, Normal) -> "τρισὶ" ++ movableNu

tettares : Adjective
tettares c g n p =
    case (c, animate g, p) of
        (Nominative, True, _) -> "τέτταρες"
        (Accusative, True, _) -> "τέτταρας"
        (Nominative, False, _) -> "τέτταρα"
        (Accusative, False, _) -> "τέτταρα"
        (Genitive, _, _) -> "τεττάρων"
        (Dative, _, Final) -> "τέτταρσι" ++ movableNu
        (Dative, _, Accented) -> "τέτταρσι" ++ movableNu
        (Dative, _, Normal) -> "τέτταρσι" ++ movableNu

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
                     Final -> "εἴκοσι" ++ movableNu
                     Accented -> "εἴκοσί" ++ movableNu
                     Normal -> "εἴκοσι" ++ movableNu

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

hapax : Adverb
hapax p = "ἅπαξ"
dis : Adverb
dis p = if accented p then "δίς" else "δὶς"
tris : Adverb
tris p = if accented p then "τρίς" else "τρὶς"

chilioi : Adjective
chilioi = chilioiS ""

conta : String -> Position -> String
conta s p =
    case p of
        Final -> s ++ "κοντα"
        Accented -> s ++ "κοντά"
        Normal -> s ++ "κοντα"

cosioi : String -> Adjective
cosioi s = vowelStemIos (s ++ "κόσ") (s ++ "κοσ")

chilioiS : String -> Adjective
chilioiS s = vowelStemIos (s ++ "χίλ") (s ++ "χιλ")

myrioiS : String -> Adjective
myrioiS s = vowelStemIos (s ++ "μύρ") (s ++ "μυρ")

cisS : String -> Adverb
cisS s p = forceAccentiateFinalA (compoundForm s) ++ "κις"

cisAS : Adjective -> Adverb
cisAS a =
    a Nominative Neuter Plural Normal
    |> cisS

cisWS : Word -> Adverb
cisWS a =
    renderWords [a]
    |> cisS

deaccentPairs : List (String, String)
deaccentPairs =
    [("άὰᾶ", "α")
    , ("ᾴᾲᾷ", "ᾳ")
    , ("ήὴῆ", "η")
    , ("ῄῂῇ", "ῃ")
    , ("ώὼῶ", "ω")
    , ("ῴῲῷ", "ῳ")
    , ("ίὶ", "ι")
    , ("ύὺ", "υ")
    , ("έὲ", "ε")
    , ("όὸ", "ο")
    , ("ἄἂἆ", "ἀ")
    , ("ᾄᾂᾆ", "ᾀ")
    , ("ἤἢἦ", "ἠ")
    , ("ᾔᾒᾖ", "ᾐ")
    , ("ὤὢὦ", "ὠ")
    , ("ᾤᾢᾦ", "ᾠ")
    , ("ἴἲ", "ἰ")
    , ("ὔὒ", "ὐ")
    , ("ἔἒ", "ἐ")
    , ("ὄὂ", "ὀ")
    , ("ἅἃἇ", "ἁ")
    , ("ᾅᾃᾇ", "ᾁ")
    , ("ἥἣἧ", "ἡ")
    , ("ᾕᾓᾗ", "ᾑ")
    , ("ὥὣὧ", "ὡ")
    , ("ᾥᾣᾧ", "ᾡ")
    , ("ἵἳ", "ἱ")
    , ("ὕὓ", "ὑ")
    , ("ἕἓ", "ἑ")
    , ("ὅὃ", "ὁ")
    , ("ΐῒ", "ϊ")
    , ("ΰῢ", "ϋ")
    ]

toGravePairs =
    [ ("ά", "ὰ")
    , ("ᾴ", "ᾲ")
    , ("ή", "ὴ")
    , ("ῄ", "ῂ")
    , ("ώ", "ὼ")
    , ("ῴ", "ῲ")
    , ("ί", "ὶ")
    , ("ύ", "ὺ")
    , ("έ", "ὲ")
    , ("ό", "ὸ")
    , ("ἄ", "ἂ")
    , ("ᾄ", "ᾂ")
    , ("ἤ", "ἢ")
    , ("ᾔ", "ᾒ")
    , ("ὤ", "ὢ")
    , ("ᾤ", "ᾢ")
    , ("ἴ", "ἲ")
    , ("ὔ", "ὒ")
    , ("ἔ", "ἒ")
    , ("ὄ", "ὂ")
    , ("ἅ", "ἃ")
    , ("ᾅ", "ᾃ")
    , ("ἥ", "ἣ")
    , ("ᾕ", "ᾓ")
    , ("ὥ", "ὣ")
    , ("ᾥ", "ᾣ")
    , ("ἵ", "ἳ")
    , ("ὕ", "ὓ")
    , ("ἕ", "ἓ")
    , ("ὅ", "ὃ")
    , ("ΐ", "ῒ")
    , ("ΰ", "ῢ")
    ]

replace : List (String, String) -> String -> String
replace pairs str =
    let replacer m =
            let s = m.match in
            case List.filter (\(pat, r) -> String.contains s pat) pairs |> List.head of
                Nothing -> s
                Just (_, r) -> r in
    let patten = "[" ++ (List.map (\(pat,_) -> pat) pairs |> String.join "") ++ "]" in
    case Regex.fromString patten of
        Nothing -> str
        Just regex ->
            Regex.replace regex replacer str

deaccent : String -> String
deaccent = replace deaccentPairs

toGrave : String -> String
toGrave = replace toGravePairs

dropMovableNu : String -> String
dropMovableNu str =
    if String.endsWith movableNu str
    then String.dropRight (String.length movableNu) str
    else str

compoundForm : String -> String
compoundForm str =
    str
    |> dropMovableNu
    |> deaccent
    |> String.replace "ς" "σ"

forceAccentiateFinalA : String -> String
forceAccentiateFinalA str =
    String.dropRight 1 str ++ "ά"

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

vowelStemOs : Bool -> String -> String -> Adjective
vowelStemOs a ss ls c g n p =
    let e s =
            if a
            then if accented p then s else toGrave s
            else deaccent s in

    case (c, g, n) of
        (Nominative, Masculine, Singular) -> ss ++ e "ός"
        (Accusative, Masculine, Singular) -> ss ++ e "όν"
        (Nominative, Neuter, Singular) -> ss ++ e "όν"
        (Accusative, Neuter, Singular) -> ss ++ e "όν"
        (Genitive, Masculine, Singular) -> ls ++ e "οῦ"
        (Genitive, Neuter, Singular) -> ls ++ e "οῦ"
        (Dative, Masculine, Singular) -> ls ++ e "ῷ"
        (Dative, Neuter, Singular) ->  ls ++ e "ῷ"

        (Nominative, Feminine, Singular) -> ls ++ e "ή"
        (Accusative, Feminine, Singular) -> ls ++ e "ήν"
        (Genitive, Feminine, Singular) -> ls ++ e "ῆς"
        (Dative, Feminine, Singular) ->  ls ++ e "ῇ"

        (Nominative, Masculine, Plural) -> ss ++ e "οί"
        (Accusative, Masculine, Plural) -> ls ++ e "ούς"
        (Nominative, Neuter, Plural) -> ss ++ e "ά"
        (Accusative, Neuter, Plural) -> ss ++ e "ά"
        (Genitive, Masculine, Plural) -> ls ++ e "ῶν"
        (Genitive, Neuter, Plural) -> ls ++ e "ῶν"
        (Dative, Masculine, Plural) -> ls ++ e "οῖς"
        (Dative, Neuter, Plural) ->  ls ++ e "οῖς"

        (Nominative, Feminine, Plural) -> ss ++ e "αί"
        (Accusative, Feminine, Plural) -> ls ++ e "άς"
        (Genitive, Feminine, Plural) -> ls ++ e "ῶν"
        (Dative, Feminine, Plural) ->  ls ++ e "αῖς"

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

hecatontaP = "ἑκατοντα"

commonCardinalFromDigits : CardinalOrder -> Case -> Gender -> List Int -> Maybe (List Word)
commonCardinalFromDigits co c g n =
    let sg w = w c g Singular |> Word in
    let pl w = w c g Plural |> Word in
    let reorder xs =
            let filtered = List.filterMap identity xs in
            case co of
              Ascending ->
                  filtered
                |> List.intersperse [cai]
                |> List.concat
              Descending ->
                  filtered
                |> List.reverse
                |> List.intersperse [cai]
                |> List.concat
              DescendingJuxtapose ->
                  filtered
                |> List.reverse
                |> List.concat
    in
    let digit zeros x =
            if x == 0
            then Just Nothing
            else
              x :: (List.repeat zeros 0)
              |> commonCardinalFromDigits Ascending c g
              |> Maybe.map Just
    in
    let compound () =
            let f (i, d) = digit i d in
            List.indexedMap digit (List.reverse n)
                |> maybesToList
                |> Maybe.map (reorder)
    in
    let fromAdverb f ds =
            case commonAdverbFromDigits ds |> Maybe.map renderWords of
                Nothing -> Nothing
                Just p -> Just [pl (f (compoundForm p))]
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
        [x,y] -> compound ()
        [1,0,0] -> Just [pl hecaton]
        [2,0,0] -> Just [pl diacosioi]
        [3,0,0] -> Just [pl triacosioi]
        [4,0,0] -> Just [pl tetracosioi]
        [5,0,0] -> Just [pl pentacosioi]
        [6,0,0] -> Just [pl hexacosioi]
        [7,0,0] -> Just [pl heptacosioi]
        [8,0,0] -> Just [pl octacosioi]
        [9,0,0] -> Just [pl enacosioi]
        [x,y,z] -> compound ()
        [1,0,0,0] -> Just [pl chilioi]
        [x,0,0,0] -> fromAdverb chilioiS [x]
        [x,y,z,w] -> compound ()
        [1, 0,0,0,0] -> Just [pl (myrioiS "")]
        [x, 0,0,0,0] -> fromAdverb myrioiS [x]
        [_, _,_,_,_] -> compound ()
        [x,0, 0,0,0,0] -> fromAdverb myrioiS [x,0]
        [_,_, _,_,_,_] -> compound ()
        [x,0,0, 0,0,0,0] -> fromAdverb myrioiS [x,0,0]
        [_,_,_, _,_,_,_] -> compound ()
        [x,0,0,0, 0,0,0,0] -> fromAdverb myrioiS [x,0,0,0]
        [_,_,_,_, _,_,_,_] -> compound ()
        [1, 0,0,0,0, 0,0,0,0] -> fromAdverb myrioiS [1, 0,0,0,0]
        [1, _,_,_,_, _,_,_,_] -> compound ()
        _ -> Nothing

commonOrdinalFromDigits : Case -> Gender -> Number -> List Int -> Maybe (List Word)
commonOrdinalFromDigits c g nu n =
    let w a ss ls = vowelStemOs a ss ls c g nu |> Word in
    let reorder xs =
            List.filterMap identity xs
            |> List.intersperse [cai]
            |> List.concat
    in
    let digit zeros x =
            if x == 0
            then Just Nothing
            else
              x :: (List.repeat zeros 0)
              |> commonOrdinalFromDigits c g nu
              |> Maybe.map Just
    in
    let compound () =
            let f (i, d) = digit i d in
            List.indexedMap digit (List.reverse n)
                |> maybesToList
                |> Maybe.map (reorder)
    in
    let stos s =
            w True (s ++ "στ") (s ++ "στ")
    in
    let ostos s =
            w True (s ++ "οστ") (s ++ "οστ")
    in
    let fromCardinal wt t =
            case commonCardinalFromDigits Ascending Nominative Neuter n |> Maybe.map renderWords of
                Nothing -> Nothing
                Just p -> Just [p |> deaccent |> String.slice 0 t |> wt] in
    case n of
        [1] -> Just [w False "πρῶτ" "πρώτ"]
        [2] -> Just [w False "δεύτερ" "δευτέρ"]
        [3] -> Just [w False "τρίτ" "τρίτ"]
        [4] -> Just [w False "τέταρτ" "τετάρτ"]
        [5] -> Just [w False "πέμπτ" "πέμπτ"]
        [6] -> Just [w False "ἕκτ" "ἕκτ"]
        [7] -> Just [w False "ἕβδομ" "ἑβδόμ"]
        [8] -> Just [w False "ὄγδο" "ὀγδό"]
        [9] -> Just [w False "ἔνατ" "ἐνάτ"]
        [1,0] -> Just [w False "δέκατ" "δεκάτ"]
        [1,1] -> Just [w False "ἑνδέκατ" "ἑνδεκάτ"]
        [1,2] -> Just [w False "δωδέκατ" "δωδεκάτ"]
        [2,0] -> Just [ostos "εἰκ"]
        [x,0] -> fromCardinal ostos -4
        [x,y] -> compound ()
        [1,0,0] -> fromCardinal ostos -2
        [_,0,0] -> fromCardinal ostos -1
        [_,_,_] -> compound ()
        [1,0,0,0] -> fromCardinal stos -1
        [_,0,0,0] -> fromCardinal ostos -1
        [_,_,_,_] -> compound ()
        [_, 0,0,0,0] -> fromCardinal ostos -1
        [_, _,_,_,_] -> compound ()
        [_,_, 0,0,0,0] -> fromCardinal ostos -1
        [_,_, _,_,_,_] -> compound ()
        [_,_,_, 0,0,0,0] -> fromCardinal ostos -1
        [_,_,_, _,_,_,_] -> compound ()
        [_,_,_,_, 0,0,0,0] -> fromCardinal ostos -1
        [_,_,_,_, _,_,_,_] -> compound ()
        [1, _,_,_,_, 0,0,0,0] -> fromCardinal ostos -1
        [1, _,_,_,_, _,_,_,_] -> compound ()
        _ -> Nothing

maybesToList : List (Maybe a) -> Maybe (List a)
maybesToList =
    let sub acc xs =
            case xs of
                [] -> Just (List.reverse acc)
                (Nothing :: xr) -> Nothing
                (Just x :: xr) -> sub (x :: acc) xr in
    sub []

commonAdverbFromDigits : List Int -> Maybe (List Word)
commonAdverbFromDigits n =
    let adv w = Word w in
    let digit zeros x =
            if x == 0
            then Just Nothing
            else
              x :: (List.repeat zeros 0)
              |> commonAdverbFromDigits
              |> Maybe.map Just
    in
    let fromCardinal () =
            case commonCardinalFromDigits Ascending Nominative Neuter n |> Maybe.map renderWords of
                Nothing -> Nothing
                Just p -> Just [cisS p |> adv] in
    let compound () =
            let f (i, d) = digit i d in
            List.indexedMap digit (List.reverse n)
            |> maybesToList
            |> Maybe.map (List.filterMap identity)
            |> Maybe.map (List.reverse)
            |> Maybe.map (List.concat) in
    case n of
        [1] -> Just [adv hapax]
        [2] -> Just [adv dis]
        [3] -> Just [adv tris]
        [4] -> Just [cisS tetraP |> adv]
        [5] -> Just [cisS pentaP |> adv]
        [6] -> Just [cisS hexaP |> adv]
        [7] -> Just [cisS heptaP |> adv]
        [8] -> Just [cisS octaP |> adv]
        [9] -> Just [cisS enaP |> adv]
        [1,3] -> Just [caideca treisP |> cisWS |> adv]
        [1,4] -> Just [caideca tettaresP |> cisWS |> adv]
        [1,x] -> fromCardinal ()
        [2,0] -> Just [eicosin |> cisAS |> adv]
        [_,0] -> fromCardinal ()
        [_,_] -> compound ()
        [1,0,0] -> Just [cisS hecatontaP |> adv]
        [_,0,0] -> fromCardinal ()
        [_,_,_] -> compound ()
        [_,0,0,0] -> fromCardinal ()
        [_,_,_,_] -> compound ()
        [_, 0,0,0,0] -> fromCardinal ()
        [_, _,_,_,_] -> compound ()
        [_,0, 0,0,0,0] -> fromCardinal ()
        [_,_, _,_,_,_] -> compound ()
        [_,0,0, 0,0,0,0] -> fromCardinal ()
        [_,_,_, _,_,_,_] -> compound ()
        [_,0,0,0, 0,0,0,0] -> fromCardinal ()
        [_,_,_,_, _,_,_,_] -> compound ()
        _ -> Nothing

plousFromDigits : Case -> Gender -> Number -> List Int -> Maybe (List Word)
plousFromDigits c g nu n =
    let fromAdverb t =
            case commonAdverbFromDigits n |> Maybe.map renderWords of
                Nothing -> Nothing
                Just p -> Just [plousP (p |> deaccent |> String.slice 0 t) c g nu |> Word] in
    let digit zeros x =
            if x == 0
            then Just Nothing
            else
              x :: (List.repeat zeros 0)
              |> commonAdverbFromDigits
              |> Maybe.map Just
    in
    let compound () =
            let f (i, d) = digit i d in
            List.indexedMap digit (List.reverse n)
            |> maybesToList
            |> Maybe.map (List.filterMap identity)
            |> Maybe.map (List.reverse)
            |> Maybe.map (List.concat) in
    case n of
        [1] -> Just [plousP "ἁ" c g nu |> Word]
        [2] -> fromAdverb -1
        [3] -> fromAdverb -1
        [9] -> Just [plousP enneaP c g nu |> Word]
        [_] -> fromAdverb -3
        [1,0] -> fromAdverb -3
        [_,0] -> fromAdverb -3
        _ -> Nothing

withDigits : (List Int -> Maybe a) -> Int -> Maybe a
withDigits f n =
    if n > 0
    then let exploded = Digits.explodeIntoDigits n in
         if List.all (\x -> x >= 0) exploded
         then f exploded
         else Nothing
    else Nothing

commonCardinal : CardinalOrder -> Case -> Gender -> Int -> Maybe (List Word)
commonCardinal co c g n =
    withDigits (commonCardinalFromDigits co c g) n

commonOrdinal : Case -> Gender -> Number -> Int -> Maybe (List Word)
commonOrdinal c g nu n =
    withDigits (commonOrdinalFromDigits c g nu) n

plous : Case -> Gender -> Number -> Int -> Maybe (List Word)
plous c g nu n =
    withDigits (plousFromDigits c g nu) n

commonAdverb : Int -> Maybe (List Word)
commonAdverb n =
    withDigits commonAdverbFromDigits n

apollonius : Case -> BigInt.BigInt -> Maybe (List Word)
apollonius c num =
    let rm = Digits.explodeIntoMyriads num |> List.reverse in
    let renderMonad i d =
            case (i, d, commonCardinal DescendingJuxtapose c Feminine d) of
                (_, 0, _) -> if i >= 0 then Just [] else Nothing
                (0, 1, Just coef) -> [monad c Singular |> Word] ++ coef |> Just
                (0, _, Just coef) -> [monad c Plural |> Word] ++ coef |> Just
                (_, _, Just coef) ->
                    plous c Feminine Plural i
                                  |> Maybe.map (\p -> p ++ [myriad c Plural |> Word] ++ coef)
                (_, _, _) -> Nothing
    in
    if BigInt.gt num Digits.zero
    then
      List.indexedMap renderMonad rm
      |> List.reverse
      |> Maybe.Extra.combine
      |> Maybe.map (List.filter (\xs -> xs /= []))
      |> Maybe.map (List.intersperse [cai])
      |> Maybe.map List.concat
    else Nothing

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

