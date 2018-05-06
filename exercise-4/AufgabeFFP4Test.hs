-- Julian Kotrba
-- 01427123

import AufgabeFFP4
import Test.HUnit
import Data.List
import Data.Array

main = runTestTT $ TestList [test1, test2]

test1 = TestLabel "Rook" $ (
    TestList [
          TestCase $ assertEqual "Rook starts on occupied field" (fuehre_zugfolge_aus Turm test_game_board (C,I) [(N,1)]) (Nothing),
  	      TestCase $ assertEqual "Rook moves through valid fields" (Just (D,I)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(N,2), (O,1), (N,2), (O,4), (S,4), (W,2)]),
          TestCase $ assertEqual "Rook should ignore bishop directions (NO, SW, NW, SO)" (Just (D,I)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(N,2), (O,1), (N,2), (O,4), (NO,1), (SW,1), (NW,1), (SO,1),(S,4), (W,2)]),
          TestCase $ assertEqual "Rook should stop before occupied field" (Just (B,III)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(N,2), (O,3), (N,2)]),
          TestCase $ assertEqual "Rook should stop when reaching the top border" (Just (B,VIII)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(N,2), (O,1), (N,6), (O,1)]),
          TestCase $ assertEqual "Rook should stop when reaching the bottom border" (Just (A,I)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(S,1), (N,1)]),
          TestCase $ assertEqual "Rook should stop when reaching the left border" (Just (A,I)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(W,1), (N,1)]),
          TestCase $ assertEqual "Rook should stop when reaching the right border" (Just (H,V)) (fuehre_zugfolge_aus Turm test_game_board (A,I) [(N,2), (O,1), (N,2), (O,7), (S,1)])
	       ])

test2 = TestLabel "Bishop" $ (TestList [
          TestCase $ assertEqual "Bishop starts on occupied field" (Nothing) (fuehre_zugfolge_aus Laeufer test_game_board (C,I) [(N,1)]),
          TestCase $ assertEqual "Bishop moves through valid fields" (Just (E,I)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(NO,1),(NW,1),(NO,3),(SO,3),(SW,2)]),
          TestCase $ assertEqual "Bishop should ignore rook directions (N, O, S, W)" (Just (E,I)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(NO,1),(NW,1),(NO,3),(SO,3), (N,2), (O,1), (S,1), (W,1), (SW,2)]),
          TestCase $ assertEqual "Bishop should stop before occupied field" (Just (B,II)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(NO,2),(NW,1)]),
          TestCase $ assertEqual "Bishop moves should have torus behaviour NW" (Just (H,II)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(NW,1)]),
          TestCase $ assertEqual "Bishop moves should have torus behaviour SO" (Just (B,VIII)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(SO,1)]),
          TestCase $ assertEqual "Bishop moves should have torus behaviour NO" (Just (A,I)) (fuehre_zugfolge_aus Laeufer test_game_board (H,VIII) [(NO,1)]),
          TestCase $ assertEqual "Bishop moves should have torus behaviour SW" (Just (H,VIII)) (fuehre_zugfolge_aus Laeufer test_game_board (A,I) [(SW,1)])
          ])

{- Game board for testing
+---+---+---+---+---+---+---+---+
| X |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| X |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
| X |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   |   |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   | X |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   | X |   |   |   |   |   |
+---+---+---+---+---+---+---+---+
|   |   | X | X | X |   | X |   |
+---+---+---+---+---+---+---+---+
|   |   | X |   |   |   |   | X |
+---+---+---+---+---+---+---+---+
-}

test_game_board = array ((A,I),(H,VIII)) [((A,I),False),((A,II),False),((A,III),False),((A,IV),False),((A,V),False),((A,VI),True),((A,VII),True),((A,VIII),True), ((B,I),False),((B,II),False),((B,III),False),((B,IV),False),((B,V),False),((B,VI),False),((B,VII),False),((B,VIII),False), ((C,I),True),((C,II),True),((C,III),True),((C,IV),True),((C,V),False),((C,VI),False),((C,VII),False),((C,VIII),False), ((D,I),False),((D,II),True),((D,III),False),((D,IV),False),((D,V),False),((D,VI),False),((D,VII),False),((D,VIII),False), ((E,I),False),((E,II),True),((E,III),False),((E,IV),False),((E,V),False),((E,VI),False),((E,VII),False),((E,VIII),False), ((F,I),False),((F,II),False),((F,III),False),((F,IV),False),((F,V),False),((F,VI),False),((F,VII),False),((F,VIII),False), ((G,I),False),((G,II),True),((G,III),False),((G,IV),False),((G,V),False),((G,VI),False),((G,VII),False),((G,VIII),False), ((H,I),True),((H,II),False),((H,III),False),((H,IV),False),((H,V),False),((H,VI),False),((H,VII),False),((H,VIII),False)]
