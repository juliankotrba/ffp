-- Julian Kotrba
-- 01427123

module AufgabeFFP7 where

type Text = String
type Word = String
type First = Int
type Last = Int

type Index = Int

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


-- Slightly improved implementation
-- The algorithm is now starting at the end of the text

test_occS = occS "abc test abcd abc" "abc"

occS :: Text -> Word -> [(First,Last)]
occS t w
	| emptyS t = []
	| emptyS w = []
	| otherwise = reverse $ occS_it ((length t)-1) t w

occS_it :: Index -> Text -> Word -> [(First, Last)]
occS_it i t w
	| i < 0 = []
	| last w == t !! i = case maybe_occ == Nothing of
												True -> occS_it (i-1) t w
												False -> (unwrap maybe_occ):occS_it (i-1) t w
	| otherwise = occS_it (i-1) t w
	where maybe_occ = occ_at_index i i t w

occ_at_index :: Index -> Last -> Text -> Word -> Maybe (First, Last)
occ_at_index i l t w
	| i < 0 && not (emptyS w)= Nothing
	| emptyS w = Just (i+1, l )
	| last w == t !! i = occ_at_index (i-1) l t (take ((length w)-1) w)
	| otherwise = Nothing


-- Boyer–Moore string search algorithm
-- TODO

-- Helper functions

emptyS :: String -> Bool
emptyS = null

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Cannot unwrap \"Nothing\""
