-- Julian Kotrba
-- 01427123

module AufgabeFFP1 where

-- Aufgabe 1

type Weight = Int -- Gewicht
type Value = Int -- Wert
type Item = (Weight,Value) -- Gegenstand als Gewichts/Wert-Paar
type Items = [Item] -- Menge der anfaenglich gegebenen Gegenstaende
type Load = [Item] -- Auswahl aus der Menge der anfaenglich gegebenen Gegenstaende; moegliche Rucksackbeladung, falls zulaessig
type Loads = [Load] -- Menge moeglicher Auswahlen
type LoadWghtVal = (Load,Weight,Value) -- Eine moegliche Auswahl mit Gesamtgewicht/-wert der Auswahl
type MaxWeight = Weight -- Hoechstzulaessiges Rucksackgewicht

rs_generator :: Items -> Loads
rs_generator a = subsequences a

rs_transformer :: Loads -> [LoadWghtVal]
rs_transformer [[]] = []
rs_transformer loads = map loadWghtValMapper loads

rs_filter :: MaxWeight -> [LoadWghtVal] -> [LoadWghtVal]
rs_filter max loadWghtVals = filter (\(_, weight, _) -> weight <= max) loadWghtVals

rs_selector1 :: [LoadWghtVal] -> [LoadWghtVal]
rs_selector1 lwv = filter (\(_, _, v) -> v == max) lwv
	where max = maxValue lwv

rs_selector2 :: [LoadWghtVal] -> [LoadWghtVal]
rs_selector2 lwv = (filterMinWeight . rs_selector1) lwv

-- Helper functions 1

filterMinWeight :: [LoadWghtVal] -> [LoadWghtVal]
filterMinWeight lwv = filter  (\(_, w, _) -> w == min) lwv
	where min = minWeight lwv

minWeight :: [LoadWghtVal] -> Value
minWeight lwv = minimum (map (\(_, w, _) -> w) lwv)

maxValue :: [LoadWghtVal] -> Value
maxValue lwv = maximum (map (\(_, _, v) -> v) lwv)

loadWghtValMapper :: Load -> LoadWghtVal
loadWghtValMapper load = (load, sumLoad load (\(x,y) -> x), sumLoad load (\(x,y) -> y))

sumLoad :: Load -> (Item -> Int) ->  Weight
sumLoad a selector = (foldr (+) 0 (map selector a))

-- TODO: just import Data.List
-- Source: https://github.com/ghc/packages-base/blob/master/Data/List.hs
-- | The 'subsequences' function returns the list of all subsequences of the argument.
--
-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> [[a]]
subsequences xs =  nonEmptySubsequences xs
--   except for the empty list.
--
-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences [] =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  	where f ys r = ys : (x : ys) : r

-- Aufgabe 2

binom :: (Integer,Integer) -> Integer
binom (n,k)
	| k==0 || n==k = 1
	| otherwise = binom (n-1,k-1) + binom (n-1,k)

-- using the multiplicative formula
-- https://en.m.wikipedia.org/wiki/Binomial_coefficient
binom_s :: (Integer,Integer) -> Integer
binom_s (n,k)
	| k > n = 0
	| otherwise = round (foldr (*) 1 (take (fromIntegral k) [ divv (fromIntegral n) i | i <- [1.0 ..]]))


binom_m :: (Integer,Integer) -> Integer
binom_m (n,k)
	| k > n = 0
	| otherwise = pascal!! (fromIntegral n)!! (fromIntegral k)

-- Real memoization
-- binom_m is also more like a stream
memo_binom :: [[Integer]]
memo_binom = [ [ binom_m2 (n,k) | k<-[0..n] ] | n<-[0..] ]

binom_m2 :: (Integer,Integer) -> Integer
binom_m2 (n,k)
  | k < 0 || n < 0 || k > n = 0
  | k == 0 || n == k	= 1
	| otherwise = (memo_binom!!(n'-1))!!(k'-1) + (memo_binom!!(n'-1))!!k'
		where
			n' = fromIntegral n
			k' = fromIntegral k

-- Helper functions 2

divv :: (Fractional a) => a -> a -> a
divv n i = (n + 1- i) / i

-- Source: http://neilmitchell.blogspot.co.at/2012/01/pascals-triangle-in-haskell.html
-- Creates pascal's triangle in a 2-Dim array
pascal = iterate next [1]
next xs = zipWith (+) ([0] ++ xs) (xs ++ [0])
