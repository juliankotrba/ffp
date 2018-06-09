-- Julian Kotrba
-- 01427123

module AufgabeFFP7 where

import Test.QuickCheck
import Data.Map (Map)
import Data.Char
import qualified Data.Map as Map

type Text = String
type Word = String
type First = Int
type Last = Int

type Index = Int
type BadMatchTable = Map Char Int

-- Simple implementation

occ :: Text -> Word -> [(First,Last)]
occ t w
	| emptyS t = []
	| emptyS w = []
	| otherwise = occ_it 0 [] t w

occ_it :: Index -> [(First, Last)] -> Text -> Word -> [(First, Last)]
occ_it i r t w
	| i >= (length t) = r
	| (w !! 0 ) == (t !! i) = occ_it (i+1) (r++found) t w
	| otherwise = occ_it (i+1) r t w
	where found = occ_word_it i i t w

occ_word_it :: Index -> First -> Text -> Word -> [(First, Last)]
occ_word_it curr_i f t w
	| emptyS w = [(f, curr_i-1 )]
	| curr_i >= (length t) = []
	| (w !! 0) == (t !! curr_i) = occ_word_it (curr_i+1) f t (tail w)
	| otherwise = []

-- Part 1
{-
Schreiben Sie eine Funktion occS :: Text -> Word -> [(First,Last)]
fur das Suchproblem, die nach folgender Idee vorgeht. Angewendet auf einen
Text t und ein Suchwort w mit Laenge n ueberprueft occS, ob die Zeichen wn−1
und tn−1 ubereinstimmen, d.h. die Zeichen von w bzw. t an den jeweiligen
Indexpositionen n − 1 (der erste Index hat jeweils Wert 0!) Falls ja,
ueberprueft occS, ob die Zeichen wn−2 und tn−2 ubereinstimmen und so weiter.
Gilt eine Uebereinstimmung schließlich auch fuer w0 und t0, so ist ein Vorkommen
gefunden und das Element (0, n − 1) zur Ergebnisliste hinzuzufugen. Tritt fuer
einen der Indizes ein Unterschied auf, so kann ausgeschlossen werden, dass ein
Suchwortvorkommen an Indexposition n − 1 im Text endet und die Suche wird in
gleicher Weise fortgesetzt an der Indexposition n im Text, d.h. ob das Suchwort
an dieser Indexposition im Text endet.
-}

occS :: Text -> Word -> [(First,Last)]
occS t w
	| emptyS t = []
	| emptyS w = []
	| otherwise = occS_it ((length w)-1) t w

occS_it :: Index -> Text -> Word -> [(First, Last)]
occS_it i t w
	| i >= length t = []
	| last w == t !! i = case maybe_occ == Nothing of
												True -> occS_it (i+1) t w
												False -> (unwrap maybe_occ):occS_it (i+1) t w
	| otherwise = occS_it (i+1) t w
	where maybe_occ = occ_at_index i i t w

occ_at_index :: Index -> Last -> Text -> Word -> Maybe (First, Last)
occ_at_index i l t w
	| i < 0 && not (emptyS w)= Nothing
	| emptyS w = Just (i+1, l )
	| last w == t !! i = occ_at_index (i-1) l t (take ((length w)-1) w)
	| otherwise = Nothing


test_occS = occS "abc test abcd abc" "abc test abcd abc abc"
--                0123456789...

-- Boyer–Moore string search algorithm

occI :: Text -> Word -> [(First,Last)]
occI t w
	| emptyS t = []
	| emptyS w = []
	| otherwise = occI_it ((length w)-1) t w (badMatchTable w)

occI_it :: Index -> Text -> Word -> BadMatchTable-> [(First, Last)]
occI_it i t w bmt
	| i >= length t = []
	| last w == t !! i = case maybe_occ == Nothing of
												True -> occI_it (i+shift_length) t w bmt
												False -> (unwrap maybe_occ):occI_it (i+shift_length) t w bmt
	| otherwise = occI_it (i+shift_length) t w bmt
	where
		maybe_occ = occ_at_index i i t w
		shift_length = let v = (Map.lookup (t !! i) bmt)
										in if v == Nothing
											then length w
											else unwrap v

test_occI = occI "abc test abcd abc" "abc"
--                0123456789...

-- Quickcheck

instance Arbitrary Char where
    arbitrary     = choose ('a', 'z')
    coarbitrary c = variant (ord c `rem` 4)

prop_coincide :: Text -> Word -> Bool
prop_coincide t w = occS t w == occI t w

-- Helper functions

emptyS :: String -> Bool
emptyS = null

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Cannot unwrap \"Nothing\""

badMatchTable :: Word -> Map Char Index
badMatchTable w =  Map.fromList (map (\p@(c,i) -> if (i == (length w)-1)
																									then (c, (length w))
																									else (c, (length w)-i-1))
																									(zip w [0.. (length w)-1]))
