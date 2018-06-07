-- Julian Kotrba
-- 01427123

module AufgabeFFP7 where

type Text = String
type Word = String
type First = Int
type Last = Int

type Index = Int

occ :: Text -> Word -> [(First,Last)]
occ t w 
	| null t = []
	| null w = []
	| otherwise = occ_it 0 [] t w

occ_it :: Index -> [(First, Last)] -> Text -> Word -> [(First, Last)]
occ_it i r t w
	| i >= (length t) = r
	| (w !! 0 ) == (t !! i) = occ_it (i+1) (r++found) t w
	| otherwise = occ_it (i+1) r t w
	where found = occ_word_it i i t w

occ_word_it :: Index -> First -> Text -> Word -> [(First, Last)] 
occ_word_it curr_i f t w
	| null w = [(f, curr_i-1 )]
	| curr_i >= (length t) = [] 
	| (w !! 0) == (t !! curr_i) = occ_word_it (curr_i+1) f t (tail w)
	| otherwise = []

