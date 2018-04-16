-- Julian Kotrba
-- 01427123

module AufgabeFFP2 where

import Data.Ix

-- Aufgabe 1

type Nat1 = Int -- Natuerliche Zahlen ab 1 als Vereinbarung
type Weight = Nat1 -- Gewicht
type Value = Nat1 -- Wert
type MaxWeight = Weight -- Hoechstzulaessiges Rucksackgewicht
type Object = (Weight,Value) -- Gegenstand als Gewichts-/Wertpaar
type Objects = [Object] -- Menge der anfaenglich gegebenen Gegenstaende
type Sol_ks = [Object] -- Auswahl aus der Menge der anfaenglich gegebenen Gegenstaende; moegliche Rucksackbeladung, falls zulaessig
type Node_ks = (Value,Weight,MaxWeight,[Object],Sol_ks)

-- 1. Filter from objects pairs with w_curr+w smaller or equal limit => list of possible succ pairs
-- 2. Map all possible objects to a new Node_ks
succ_ks :: Node_ks -> [Node_ks]
succ_ks (vcurr,wcurr,limit,objects,psol) = map (\o@(w,v) -> (vcurr+v, wcurr+w, limit, rm o objects, o:psol)) (possible_objects objects wcurr limit)

-- Find minimum w in not assigned objects and check if w_min + wcurr > limit
-- If this holds it holds for all not assigned objects
goal_ks :: Node_ks -> Bool
goal_ks (_,_,_,[],_) = True -- all objects assigned
goal_ks (_,w,limit,os@((w',_):_),_) = (minimum (map (\(w1,_) -> w1) os ))+w > limit

-- 1. Get all possible solutions
-- 2. Filter solutions with max value
-- 3. Take first Node
-- 4. Map to (Sol_ks, Value)
knapsack :: Objects -> MaxWeight -> (Sol_ks,Value)
knapsack objects limit = a
	where a = map_node_ks $ head $ max_value_filter (search_dfs succ_ks goal_ks (0, 0, limit, objects, []))

map_node_ks :: Node_ks -> (Sol_ks, Value)
map_node_ks (v,_,__,_, os) = (os, v)

max_value_filter :: [Node_ks] -> [Node_ks]
max_value_filter ns = filter (\(v,_,_,_,_) -> v==max) ns
	where max = max_value ns

max_value :: [Node_ks] -> Value
max_value ns = maximum (map (\(v,_,_,_,_) -> v) ns)

-- Find objects which do not exceed the limit
possible_objects :: [Object] -> Weight -> Weight -> [Object]
possible_objects os wcurr limit = filter (\(w,_) -> wcurr+w <= limit) os

-- Helper for removing just first occurrence of Object from [Object]
rm :: Object -> [Object] -> [Object]
rm _ [] = []
rm o (x:xs) 
	| o==x = xs
	| otherwise = x:rm o xs 
		

-- Stack
data Stack a = Empty | Stk a (Stack a)

emptyS = Empty

is_emptyS Empty = True
is_emptyS _ = False 

push x s = Stk x s

pop Empty = error "Stack is empty" 
pop (Stk _ s) =s

top Empty = error "Stack is empty" 
top(Stk x _) = x


search_dfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
search_dfs succ goal n = (search (push n emptyS))
	where search s -- s for stack
       		| is_emptyS s  = []
       		| goal (top s) = top s : search (pop s)
       		| otherwise
            		= let m = top s
              			in search (foldr push (pop s) (succ m))


-- Aufgabe 2

-- Return the nth element from index m
binom_dp :: (Integer,Integer) -> Integer
binom_dp (m,n) 
	| n > m = 0
	| m < 0 || n < 0 = 0
	| otherwise = (findT t m)!!(fromIntegral n)
	    where t = dynamic comp_b (bnds_b m)

dynamic :: (Ix coord) => (Table entry coord -> coord -> entry) -> (coord,coord) -> (Table entry coord) 
dynamic compute bnds = t
	where t = newT (map (\coord -> (coord, compute t coord)) (range bnds))

{-
 computing:
        1: 1
	2: 1 1
	3: 1 2  1
	4: 1 3  3  1 
	5: 1 4  6  4 1 1
	6: 1 5 10 10 5 1
	m: ...
-}
comp_b :: Table [Integer] Integer -> Integer -> [Integer]
comp_b t i 
	| i == 0 = [1]
	| otherwise = [1] ++ [(findT t (i-1))!! fromIntegral k + (findT t (i-1))!! fromIntegral(k-1)  | k<-[1..i-1]] ++ [1]

bnds_b :: Integer -> (Integer, Integer)
bnds_b n = (0,n)

-- Table
newtype Table a b = Tbl [(b,a)]

newT t = Tbl t

findT (Tbl []) i = error "Item not found"
findT (Tbl ((j,v):r)) i
   | i==j      = v
   | otherwise = findT (Tbl r) i

updT e (Tbl []) = Tbl [e]
updT e'@(i,_) (Tbl (e@(j,_):r))
	| i==j = Tbl (e':r)
	| otherwise = Tbl (e:r') 
		where Tbl r' = updT e' (Tbl r)

