module Inflection exposing (InflectionData, Number(..), Case(..), Gender(..), renderInflectionEn, parseInflection, emptyInflection)

import BigInt
import Dict

type Number = Singular | Plural
type Gender = Masculine | Neuter | Feminine
type Case = Nominative | Genitive | Dative | Accusative

type alias InflectionData =
    { number: Maybe Number
    , gender: Maybe Gender
    , case_: Maybe Case
    }

emptyInflection =
    { number=Nothing
    , gender=Nothing
    , case_=Nothing
    }

renderInflectionEn : InflectionData -> String
renderInflectionEn i =
    [ case i.case_ of
        Nothing -> Nothing
        Just case_ ->
          case case_ of
            Nominative -> Just "nom."
            Accusative -> Just "acc."
            Genitive -> Just "gen."
            Dative -> Just "dat."
    , case i.gender of
        Nothing -> Nothing
        Just gender ->
          case gender of
            Masculine -> Just "masc."
            Feminine -> Just "fem."
            Neuter -> Just "neut."
    , case i.number of
        Nothing -> Nothing
        Just number ->
          case number of
            Singular -> Just "sg."
            Plural -> Just "pl."
    ]
    |> List.filterMap identity
    |> String.join " "

parseInflection : String -> Maybe InflectionData
parseInflection l =
    l
    |> String.split " "
    |> List.foldl updateInflectionWords (Just emptyInflection)

updateInflectionWords : String -> Maybe InflectionData -> Maybe InflectionData
updateInflectionWords l md =
    case (l, md) of
        (_, Nothing) -> Nothing
        ("nominative", Just d) -> Just {d | case_=Just Nominative}
        ("nom.", Just d) -> Just {d | case_=Just Nominative}
        ("accusative", Just d) -> Just {d | case_=Just Accusative}
        ("acc.", Just d) -> Just {d | case_=Just Accusative}
        ("genitive", Just d) -> Just {d | case_=Just Genitive}
        ("gen.", Just d) -> Just {d | case_=Just Genitive}
        ("dative", Just d) -> Just {d | case_=Just Dative}
        ("dat.", Just d) -> Just {d | case_=Just Dative}
        ("singular", Just d) -> Just {d | number=Just Singular}
        ("sing.", Just d) -> Just {d | number=Just Singular}
        ("sg.", Just d) -> Just {d | number=Just Singular}
        ("plural", Just d) -> Just {d | number=Just Plural}
        ("plur.", Just d) -> Just {d | number=Just Plural}
        ("pl.", Just d) -> Just {d | number=Just Plural}
        ("masculine", Just d) -> Just {d | gender=Just Masculine}
        ("masc.", Just d) -> Just {d | gender=Just Masculine}
        ("m.", Just d) -> Just {d | gender=Just Masculine}
        ("feminine", Just d) -> Just {d | gender=Just Feminine}
        ("fem.", Just d) -> Just {d | gender=Just Feminine}
        ("f.", Just d) -> Just {d | gender=Just Feminine}
        ("neuter", Just d) -> Just {d | gender=Just Neuter}
        ("neut.", Just d) -> Just {d | gender=Just Neuter}
        ("n.", Just d) -> Just {d | gender=Just Neuter}
        ("", Just d) -> Just d
        (_, _) -> Nothing
