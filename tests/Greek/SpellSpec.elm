module Greek.SpellSpec exposing (..)

import BigInt
import Dict exposing (Dict)
import Greek.Spell exposing (Gender(..), Case(..), Number(..), CardinalOrder(..))
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

commonCardinalNomMasc : Dict Int String
commonCardinalNomMasc = Dict.fromList
    [ (1, "εἷς")
    , (2, "δύο")
    , (3, "τρεῖς")
    , (4, "τέτταρες")
    , (5, "πέντε")
    , (6, "ἕξ")
    , (7, "ἑπτά")
    , (8, "ὀκτώ")
    , (9, "ἐννέα")
    , (10, "δέκα")
    , (11, "ἕνδεκα")
    , (12, "δώδεκα")
    , (13, "τρεῖς καὶ δέκα")
    , (14, "τέτταρες καὶ δέκα")
    , (15, "πεντεκαίδεκα")
    , (16, "ἑκκαίδεκα")
    , (17, "ἑπτακαίδεκα")
    , (18, "ὀκτωκαίδεκα")
    , (19, "ἐννεακαίδεκα")
    , (20, "εἴκοσι(ν)")
    , (21, "εἷς καὶ εἴκοσι(ν)")
    , (30, "τριάκοντα")
    , (40, "τετταράκοντα")
    , (50, "πεντήκοντα")
    , (60, "ἑξήκοντα")
    , (70, "ἑβδομήκοντα")
    , (80, "ὀγδοήκοντα")
    , (90, "ἐνενήκοντα")
    , (100, "ἑκατόν")
    , (200, "διακόσιοι")
    , (300, "τριακόσιοι")
    , (400, "τετρακόσιοι")
    , (500, "πεντακόσιοι")
    , (600, "ἑξακόσιοι")
    , (700, "ἑπτακόσιοι")
    , (800, "ὀκτακόσιοι")
    , (900, "ἐνακόσιοι")
    , (1000, "χίλιοι")
    , (2000, "δισχίλιοι")
    , (3000, "τρισχίλιοι")
    , (10000, "μύριοι")
    , (20000, "δισμύριοι")
    , (100000, "δεκακισμύριοι")
    , (100000000, "μυριακισμύριοι")
    -- , (1230000, "τρισμύριοι καὶ εἰκοσακισμύριοι καὶ ἑκατοντακισμύριοι")
    ]

commonOrdinalNomMasc : Dict Int String
commonOrdinalNomMasc = Dict.fromList
    [ (1, "πρῶτος")
    , (2, "δεύτερος")
    , (3, "τρίτος")
    , (4, "τέταρτος")
    , (5, "πέμπτος")
    , (6, "ἕκτος")
    , (7, "ἕβδομος")
    , (8, "ὄγδοος")
    , (9, "ἔνατος")
    , (10, "δέκατος")
    , (11, "ἑνδέκατος")
    , (12, "δωδέκατος")
    , (13, "τρίτος καὶ δέκατος")
    , (14, "τέταρτος καὶ δέκατος")
    , (15, "πέμπτος καὶ δέκατος")
    , (16, "ἕκτος καὶ δέκατος")
    , (17, "ἕβδομος καὶ δέκατος")
    , (18, "ὄγδοος καὶ δέκατος")
    , (19, "ἔνατος καὶ δέκατος")
    , (20, "εἰκοστός")
    , (21, "πρῶτος καὶ εἰκοστός")
    , (30, "τριακοστός")
    , (40, "τετταρακοστός")
    , (50, "πεντηκοστός")
    , (60, "ἑξηκοστός")
    , (70, "ἑβδομηκοστός")
    , (80, "ὀγδοηκοστός")
    , (90, "ἐνενηκοστός")
    , (100, "ἑκατοστός")
    , (200, "διακοσιοστός")
    , (300, "τριακοσιοστός")
    , (400, "τετρακοσιοστός")
    , (500, "πεντακοσιοστός")
    , (600, "ἑξακοσιοστός")
    , (700, "ἑπτακοσιοστός")
    , (800, "ὀκτακοσιοστός")
    , (900, "ἐνακοσιοστός")
    , (1000, "χιλιστός")
    , (2000, "δισχιλιοστός")
    , (3000, "τρισχιλιοστός")
    , (10000, "μυριοστός")
    , (20000, "δισμυριοστός")
    , (100000, "δεκακισμυριοστός")
    , (100000000, "μυριακισμυριοστός")
    ]

plousNomMasc : Dict Int String
plousNomMasc = Dict.fromList
    [ (1, "ἁπλοῦς")
    , (2, "διπλοῦς")
    , (3, "τριπλοῦς")
    , (4, "τετραπλοῦς")
    , (5, "πενταπλοῦς")
    , (6, "ἑξαπλοῦς")
    , (7, "ἑπταπλοῦς")
    , (8, "ὀκταπλοῦς")
    , (9, "ἐννεαπλοῦς")
    , (10, "δεκαπλοῦς")
    ]

apolloniusNomMasc : Dict String String
apolloniusNomMasc = Dict.fromList
    [ ("1", "μονὰς μία")
    , ("2", "μονάδες δύο")
    , ("10", "μονάδες δέκα")
    , ("10000", "ἁπλαῖ μυριάδες μία")
    , ("10001", "ἁπλαῖ μυριάδες μία καὶ μονὰς μία")
    , ("10101", "ἁπλαῖ μυριάδες μία καὶ μονάδες ἑκατὸν μία")
    , ("20000", "ἁπλαῖ μυριάδες δύο")
    , ("20001", "ἁπλαῖ μυριάδες δύο καὶ μονὰς μία")
    , ("20101", "ἁπλαῖ μυριάδες δύο καὶ μονάδες ἑκατὸν μία")
    , ("100000", "ἁπλαῖ μυριάδες δέκα")
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

        , describe "Greek.Spell.commonCardinal"
              [ test "test by golden set (Ascending)" <|
                    \_ ->
                    commonCardinalNomMasc
                    |> Dict.map (\k _ -> Greek.Spell.commonCardinal Ascending Nominative Masculine k |> Maybe.map Greek.Spell.renderWords)
                    |> Expect.equalDicts (Dict.map (\_ -> Just) commonCardinalNomMasc)
              ]

        , describe "Greek.Spell.commonOrdinal"
              [ test "test by golden set" <|
                    \_ ->
                    commonOrdinalNomMasc
                    |> Dict.map (\k _ -> Greek.Spell.commonOrdinal Nominative Masculine Singular k |> Maybe.map Greek.Spell.renderWords)
                    |> Expect.equalDicts (Dict.map (\_ -> Just) commonOrdinalNomMasc)
              ]

        , describe "Greek.Spell.plous"
              [ test "test by golden set" <|
                    \_ ->
                    plousNomMasc
                    |> Dict.map (\k _ -> Greek.Spell.plous Nominative Masculine Singular k |> Maybe.map Greek.Spell.renderWords)
                    |> Expect.equalDicts (Dict.map (\_ -> Just) plousNomMasc)
              ]

        , describe "Greek.Spell.apollonius"
              [ test "test by golden set" <|
                    \_ ->
                    apolloniusNomMasc
                    |> Dict.map (\k _ ->
                                     BigInt.fromIntString k
                                |> Maybe.andThen (Greek.Spell.apollonius Nominative)
                                |> Maybe.map Greek.Spell.renderWords)
              |> Expect.equalDicts (Dict.map (\_ -> Just) apolloniusNomMasc)
              ]

        ]
