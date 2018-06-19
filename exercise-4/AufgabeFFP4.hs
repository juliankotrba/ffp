-- Julian Kotrba
-- 01427123

module AufgabeFFP4 where

import Data.Array

-- Modellierung Schachfiguren
data Schachfigur = Turm | Laeufer deriving (Eq,Show)

-- Modellierung Zugrichtung
data Zugrichtung = N | O | S | W | NW | NO | SO | SW deriving (Eq,Show)

-- Modellierung Laenge eines Zugs in Anzahl von Feldern
type Zuglaenge = Int

-- Modellierung Schachbrett
data Zeile = I | II | III | IV | V | VI | VII | VIII deriving (Eq,Ord,Enum,Bounded,Show,Ix)
data Reihe = A | B | C | D | E | F | G | H deriving (Eq,Ord,Enum,Bounded,Show,Ix)

type Besetzt = Bool

type Schachbrett = Array (Reihe,Zeile) Bool
type Schachbrettfeld = (Reihe,Zeile)
type Ausgangsfeld = Schachbrettfeld
type Endfeld = Schachbrettfeld
type Zug = (Zugrichtung,Zuglaenge)
type Zugfolge = [Zug]

data GameState = GameState {
	curr :: Schachbrettfeld,
	game_board :: Schachbrett,
	figure :: Schachfigur
} deriving Show

newtype Move a = Move (GameState -> (a, GameState))

instance Monad Move where
	return x = Move (\s -> (x, s))

  	(Move m1) >>= g = Move $ \ s0 ->
                       let (r1, s1) = m1 s0 in
                       let (Move m2)  = g  r1 in
                       m2 s1

fuehre_zugfolge_aus :: Schachfigur -> Schachbrett -> Ausgangsfeld -> Zugfolge -> Maybe Endfeld
fuehre_zugfolge_aus f b s ms
	| is_occupied b s = Nothing
	| otherwise = Just end
	where end = move f b s ms

fuehre_zugfolge_aus_mf :: Schachfigur -> Schachbrett -> Ausgangsfeld -> Zugfolge -> Maybe Endfeld
fuehre_zugfolge_aus_mf f b s ms
	| is_occupied b s = Nothing
	| otherwise = Just end
	where end = curr $ snd $ (get_move_fun (mapM map_zug_move ms)) (GameState s b f)

move :: Schachfigur -> Schachbrett -> Schachbrettfeld -> Zugfolge -> Endfeld
move f b curr_f ms
	| null ms = curr_f
	| otherwise = move f b next_field (tail ms)
	where 	is_valid_func 	= if f == Turm then is_valid_rook else is_valid_bishop
		next_func 	= if f == Turm then next_rook else next_bishop
		next_field = move_f b curr_f (head ms) is_valid_func next_func

move_f :: Schachbrett -> Schachbrettfeld -> Zug -> (Schachbrettfeld -> Zug -> Bool) -> (Schachbrettfeld -> Zugrichtung -> Schachbrettfeld) -> Schachbrettfeld
move_f b curr@(col,row) m@(d,l) is_valid_func next_func
	| l < 0 = move_f b curr (revert d, negate l) is_valid_func next_func
	| l == 0 = curr
	| not (is_valid_func curr m) = curr
	| is_occupied b next_field = curr
	| otherwise = move_f b next_field (d,l-1) is_valid_func next_func
	where next_field = next_func curr d

get_move_fun :: Move a -> (GameState -> (a, GameState))
get_move_fun (Move f) = f

update_state :: (GameState -> GameState) -> Move ()
update_state f = Move (\s -> ((), f s))

map_zug_move :: Zug -> Move ()
map_zug_move z = (update_state update)
	where update s =
	       	let is_valid_func = if (figure s) == Turm then is_valid_rook else is_valid_bishop
		    next_func     = if (figure s) == Turm then next_rook else next_bishop
	        in s {curr = move_f (game_board s) (curr s) z is_valid_func next_func}

-- validation if next move is valid (rook)
is_valid_rook :: Schachbrettfeld -> Zug -> Bool
is_valid_rook (col,row) (d,l)
	| row == VIII && d == N = False
        | row == I && d == S = False
        | col == A && d == W = False
        | col == H && d == O = False
	| otherwise = True

-- validation if next move is valid (bishop)
-- always true because bishop sees the game board as a torus
is_valid_bishop :: Schachbrettfeld -> Zug -> Bool
is_valid_bishop f m = True

-- calculates the next field for a bishop
next_bishop :: Schachbrettfeld -> Zugrichtung -> Schachbrettfeld
next_bishop (col, row) NW = (predB col ,succB row)
next_bishop (col, row) NO = (succB col, succB row)
next_bishop (col, row) SO = (succB col, predB row)
next_bishop (col, row) SW = (predB col, predB row)
next_bishop (col, row) N = (col, row)
next_bishop (col, row) O = (col, row)
next_bishop (col, row) S = (col, row)
next_bishop (col, row) W = (col, row)

-- calculates the next field for a rook
next_rook :: Schachbrettfeld -> Zugrichtung -> Schachbrettfeld
next_rook (col, row) N = (col, succ row)
next_rook (col, row) S = (col, pred row)
next_rook (col, row) O = (succ col, row)
next_rook (col, row) W = (pred col, row)
next_rook (col, row) NW = (col, row)
next_rook (col, row) NO = (col, row)
next_rook (col, row) SO = (col, row)
next_rook (col, row) SW = (col, row)

succB :: (Bounded a, Enum a, Eq a) => a -> a
succB en | en == maxBound = minBound
         | otherwise = succ en

predB :: (Bounded a, Enum a, Eq a) => a -> a
predB en | en == minBound = maxBound
         | otherwise = pred en

revert :: Zugrichtung -> Zugrichtung
revert N = S
revert O = W
revert S = N
revert W = O
revert NW = SO
revert NO = SW
revert SO = NW
revert SW = NO

is_occupied :: Schachbrett -> Schachbrettfeld -> Bool
is_occupied b f = b ! f


-- test values

board = array ((A,I),(H,VIII)) [((A,I),False),((A,II),False),((A,III),False),((A,IV),False),((A,V),False),((A,VI),True),((A,VII),True),((A,VIII),True), ((B,I),False),((B,II),False),((B,III),False),((B,IV),False),((B,V),False),((B,VI),False),((B,VII),False),((B,VIII),False), ((C,I),True),((C,II),True),((C,III),True),((C,IV),True),((C,V),False),((C,VI),False),((C,VII),False),((C,VIII),False), ((D,I),False),((D,II),True),((D,III),False),((D,IV),False),((D,V),False),((D,VI),False),((D,VII),False),((D,VIII),False), ((E,I),False),((E,II),True),((E,III),False),((E,IV),False),((E,V),False),((E,VI),False),((E,VII),False),((E,VIII),False), ((F,I),False),((F,II),False),((F,III),False),((F,IV),False),((F,V),False),((F,VI),False),((F,VII),False),((F,VIII),False), ((G,I),False),((G,II),True),((G,III),False),((G,IV),False),((G,V),False),((G,VI),False),((G,VII),False),((G,VIII),False), ((H,I),True),((H,II),True),((H,III),False),((H,IV),False),((H,V),False),((H,VI),False),((H,VII),False),((H,VIII),False)]

m1 = (N, 5)
