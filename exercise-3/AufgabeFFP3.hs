-- Julian Kotrba
-- 01427123

module AufgabeFFP3 where

import Data.List
import Data.Array

-- Aufgabe 1

search_dfso :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> node
search_dfso succ goal n = (search (push n emptyS))
	where search s -- s for stack
        	| is_emptyS s  = n -- []
                | goal (top s) = top s  --  : search (pop s)
                | otherwise
                	= let m = top s
                        	in search (foldr push (pop s) (succ m))

-- Stack
newtype Stack a = Stk [a]
emptyS = Stk []

is_emptyS (Stk []) = True
is_emptyS (Stk _)  = False

push x (Stk xs) = Stk (x:xs)

pop (Stk []) = error "Stack is empty"
pop (Stk (_:xs)) = Stk xs

top (Stk []) = error "Stack is empty"
top (Stk (x:_)) = x


-- Aufgabe 2

data Zahl = Null | Eins | Zwei | Drei | Vier | Fuenf deriving (Eq,Ord,Enum,Show)
data Zeile = I | II | III | IV | V | VI | VII | VIII deriving (Eq,Ord,Enum,Show, Ix)
data Reihe = A | B | C | D | E | F | G | H deriving (Eq,Ord,Enum,Show, Ix)

type Besetzt = Bool

type Schachbrett = Array (Reihe,Zeile) Besetzt

type Zugzahl = Zahl
type Feld = (Reihe,Zeile)
type Von = Feld
type Nach = Feld
type Startfeld = Von
type Zielfeld = Nach
type Aufgabe = (Startfeld,Zielfeld,Zugzahl)
type Zug = (Von,Nach)
type Zugfolge = [Zug]

-- TODO: Use record syntax
data Knoten = Knoten
	Feld 					-- current field
	Feld 					-- goal field
	Zugzahl 			-- remaining moves
	Zugfolge 			-- moves
	Schachbrett 	-- board
	deriving (Show,Eq)

suche :: Schachbrett -> Aufgabe -> Zugfolge
suche sb (s,e,z)
	| sb ! s = []
	| sb ! e = []
	| otherwise = moves
		where
			moves = extract_moves (search_dfso nachf lsg start_node)
			start_node = (Knoten s e z [] sb)


extract_moves :: Knoten -> Zugfolge
extract_moves (Knoten _ _ _  ms _) = ms

lsg :: Knoten -> Bool
lsg (Knoten curr goal mcnt _ _) = mcnt == Null && curr == goal

nachf :: Knoten -> [Knoten]
nachf (Knoten _ _ Null _ _ ) = []
nachf n@(Knoten f goal count ms board) =  rows ++ columns
		where
			rows = succ_rows n
			columns = succ_columns n

succ_rows :: Knoten -> [Knoten]
succ_rows n@(Knoten cur@(column,row) goal count ms board) = remove_occupied_above n all_succs ++ remove_occupied_below n (reverse all_succs)
		where all_succs = succs_2_nodes n (filter (/= cur) (map (\r -> (column, r)) $ range (I,VIII)))

succ_columns :: Knoten -> [Knoten]
succ_columns n@(Knoten cur@(column,row) goal count ms board) = remove_occupied_right n all_succs ++ remove_occupied_left n (reverse all_succs)
		where all_succs = succs_2_nodes n (filter (/= cur) (map (\c -> (c, row))$ range (A,H)))

succs_2_nodes :: Knoten -> [Feld] -> [Knoten]
succs_2_nodes (Knoten curr goal count ms board) fs = map (\f -> Knoten f goal (pred count) (ms++[(curr,f)]) board) fs


-- TODO: Refactor all remove functions (above, below, right, left)

remove_occupied_above :: Knoten -> [Knoten] -> [Knoten]
remove_occupied_above (Knoten (column,VIII) _ _ _ _) ns = [] -- no fields above
remove_occupied_above _ [] = []
remove_occupied_above curr@(Knoten cur@(currcolumn,currrow) _ _ _ _) (n@(Knoten f@(column,row) goal count ms board):ns)
		| row <= currrow = remove_occupied_above curr ns
		| (board ! f) = []
		| otherwise = n : remove_occupied_above curr ns

remove_occupied_below :: Knoten -> [Knoten] -> [Knoten]
remove_occupied_below (Knoten (column,I) _ _ _ _) ns = [] -- no fields below
remove_occupied_below _ [] = []
remove_occupied_below curr@(Knoten cur@(currcolumn,currrow) _ _ _ _) (n@(Knoten f@(column,row) goal count ms board):ns)
		| row >= currrow = remove_occupied_below curr ns
		| (board ! f) = []
		| otherwise = n:(remove_occupied_below curr ns)

remove_occupied_right :: Knoten -> [Knoten] -> [Knoten]
remove_occupied_right (Knoten (H,row) _ _ _ _) ns = [] -- no fields to the right
remove_occupied_right _ [] = []
remove_occupied_right curr@(Knoten cur@(currcolumn,currrow) _ _ _ _) (n@(Knoten f@(column,row) goal count ms board):ns)
		| column <= currcolumn = remove_occupied_right curr ns
		| (board ! f) = []
		| otherwise = n : remove_occupied_right curr ns

remove_occupied_left :: Knoten -> [Knoten] -> [Knoten]
remove_occupied_left (Knoten (A,row) _ _ _ _) ns = [] -- no fields to the left
remove_occupied_left _ [] = []
remove_occupied_left curr@(Knoten cur@(currcolumn,currrow) _ _ _ _) (n@(Knoten f@(column,row) goal count ms board):ns)
		| column >= currcolumn = remove_occupied_left curr ns
		| (board ! f) = []
		| otherwise = n : remove_occupied_left curr ns


-- test implementation with predefined values

test = suche board task

board = array ((A,I),(H,VIII)) [((A,I),False),((A,II),True),((A,III),False),((A,IV),True),((A,V),True),((A,VI),True),((A,VII),True),((A,VIII),True), ((B,I),False),((B,II),False),((B,III),False),((B,IV),False),((B,V),False),((B,VI),False),((B,VII),False),((B,VIII),False), ((C,I),True),((C,II),True),((C,III),True),((C,IV),True),((C,V),True),((C,VI),True),((C,VII),True),((C,VIII),True), ((D,I),False),((D,II),True),((D,III),False),((D,IV),False),((D,V),False),((D,VI),False),((D,VII),False),((D,VIII),False), ((E,I),False),((E,II),True),((E,III),False),((E,IV),False),((E,V),False),((E,VI),False),((E,VII),False),((E,VIII),False), ((F,I),False),((F,II),False),((F,III),False),((F,IV),False),((F,V),False),((F,VI),False),((F,VII),False),((F,VIII),False), ((G,I),False),((G,II),True),((G,III),False),((G,IV),False),((G,V),False),((G,VI),False),((G,VII),False),((G,VIII),False), ((H,I),True),((H,II),True),((H,III),False),((H,IV),False),((H,V),False),((H,VI),False),((H,VII),False),((H,VIII),False)]

task = ((B,VI),(A,III), Drei)
