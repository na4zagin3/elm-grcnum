module Greek.SpellSpec exposing (..)

import BigInt
import Dict exposing (Dict)
import Greek.Spell
import Expect exposing (Expectation, pass, fail)
import Fuzz exposing (Fuzzer, int, list, string, tuple, tuple3)
import Test exposing (..)

commonAdverbDict : Dict Int String
commonAdverbDict = Dict.fromList
    [ (1, "ἅπαξ")
    , (2, "δίς")
    , (3, "τρίς")
    , (4, "τετράκις")
    , (5, "πεντάκις")
    , (6, "ἑξάκις")
    , (7, "ἑπτάκις")
    , (8, "ὀκτάκις")
    , (9, "ἐνάκις")
    , (10, "δεκάκις")
    , (11, "ἑνδεκάκις")
    , (12, "δωδεκάκις")
    , (13, "τρεισκαιδεκάκις")
    , (14, "τετταρεσκαιδεκάκις")
    , (15, "πεντεκαιδεκάκις")
    , (16, "ἑκκαιδεκάκις")
    , (17, "ἑπτακαιδεκάκις")
    , (18, "ὀκτωκαιδεκάκις")
    , (19, "ἐννεακαιδεκάκις")
    , (20, "εἰκοσάκις")
    , (21, "εἰκοσάκις ἅπαξ")
    , (30, "τριακοντάκις")
    , (40, "τετταρακοντάκις")
    , (50, "πεντηκοντάκις")
    , (60, "ἑξηκοντάκις")
    , (70, "ἑβδομηκοντάκις")
    , (80, "ὀγδοηκοντάκις")
    , (90, "ἐνενηκοντάκις")
    , (100, "ἑκατοντάκις")
    , (200, "διακοσιάκις")
    , (300, "τριακοσιάκις")
    , (400, "τετρακοσιάκις")
    , (500, "πεντακοσιάκις")
    , (600, "ἑξακοσιάκις")
    , (700, "ἑπτακοσιάκις")
    , (800, "ὀκτακοσιάκις")
    , (900, "ἐνακοσιάκις")
    , (1000, "χιλιάκις")
    , (2000, "δισχιλιάκις")
    , (3000, "τρισχιλιάκις")
    , (10000, "μυριάκις")
    , (20000, "δισμυριάκις")
    , (100000, "δεκακισμυριάκις")
    ]

suite : Test
suite =
    describe "Greek.Spell"
        [ describe "Greek.Spell.commonAdverb"
              [ test "test by golden set" <|
                    \_ ->
                    commonAdverbDict
                    |> Dict.map (\k _ -> Greek.Spell.commonAdverb k |> Maybe.map Greek.Spell.renderWords)
                    |> Expect.equalDicts (Dict.map (\_ -> Just) commonAdverbDict)
              ]

        ]
