-- Julian Kotrba
-- 01427123

module AufgabeFFP8 where

import Data.Array
import Data.List

data Content = Tree | Tent | Empty deriving (Eq,Ord,Show)

type Camp = Array (Int,Int) Content
type ChoicesCamp = Array (Int,Int) Choices

type Row = Int -- ausschliesslich Werte von 1 bis 8
type Column = Int -- ausschliesslich Werte von 1 bis 8
type LocationsOfTrees = [(Row,Column)]

type TentsPerRow = [Int] -- Liste der Laenge 8, ausschliesslich
type TentsPerColumn = [Int] -- Liste der Laenge 8, ausschliesslich

type Choices = [Content]
type Grid = [[Choices]]
type Index = Int

simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp ts tpr tpc = head $ filter (\x-> valid x tpr tpc) $ expand $ removeTreelessTents $ choices $ makeCamp ts

-- calculates all possible choice for a camping place
choices :: Camp -> ChoicesCamp
choices = fmap choice

choice d = if d == Empty then [Tent, Empty] else [d]

expand :: ChoicesCamp -> [Camp]
expand a = map (\x->listArray ((0,0),(7,7)) (concat x)) ((cp . fmap cp) (splitEvery 8 $ elems a))

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

valid :: Camp -> TentsPerRow -> TentsPerColumn -> Bool
valid c tpr tpc = (map toTentCount (rows c)) == tpr &&
                  (map toTentCount (cols c)) == tpc &&
                  (isTentPerTree c) &&
                  (not (hasTentContact c))

toTentCount :: [Content] -> Int
toTentCount cs = length (filter (\c -> c == Tent) cs)

tentCountOk :: [Content] -> Int -> Bool
tentCountOk cs m = length (filter (\c -> c == Tent) cs) == m

cols :: Camp -> [[Content]]
cols c = [[c ! (j, i) | j<-[0..7] ] | i <- [0..7]]

rows :: Camp -> [[Content]]
rows c = [[c ! (i, j) | j<-[0..7] ] | i <- [0..7]]

isTentPerTree :: Camp -> Bool
isTentPerTree c = isTentPerTreeHelper (elems c) 0

isTentPerTreeHelper :: [Content] -> Index -> Bool
isTentPerTreeHelper cs i
  | i == (length cs)-1 = if cs!!i == Tree then hasTent else True
  | cs!!i == Tree = hasTent && isTentPerTreeHelper cs (i+1)
  | otherwise = isTentPerTreeHelper cs (i+1)
  where hasTent = (isTent (i-1) cs) || -- check left
                    (isTent (i+1) cs) || -- check right
                    (isTent (i+8) cs) || -- check top
                    (isTent (i-8) cs)    -- check bottom

isTent :: Index -> [Content] -> Bool
isTent i cs
  | i < 0 || i >= length cs = False
  | otherwise = cs !! i == Tent

hasTentContact :: Camp -> Bool
hasTentContact c = hasTentContactHelper (elems c) 0

hasTentContactHelper :: [Content] -> Index -> Bool
hasTentContactHelper cs i
  | i == (length cs)-1 = if cs!!i == Tree then (hasContact) else True
  | cs !! i == Tent = (hasContact && hasTentContactHelper cs (i+1))
  | otherwise = hasTentContactHelper cs (i+1)
  where hasContact = (isTent (i-1) cs)  || -- check left
                      (isTent (i+1) cs) || -- check right
                      (isTent (i-8) cs) || -- check top
                      (isTent (i+8) cs) || -- check bottom
                      (isTent (i-9) cs) || -- check top left corner
                      (isTent (i-7) cs) || -- check top right corner
                      (isTent (i+9) cs) || -- check bottom left corner
                      (isTent (i+8) cs)    -- check bottom right corner


removeTreelessTents :: ChoicesCamp -> ChoicesCamp
removeTreelessTents cc = listArray ((0,0),(7,7)) ((map (\(i,cs) -> if (length cs == 1) || (hasTent i) then cs else [Empty]) (zip [0..length ccList] ccList)))
  where hasTent i = (isTree (i-1) ccList) || -- check left
                    (isTree (i+1) ccList) || -- check right
                    (isTree (i+8) ccList) || -- check top
                    (isTree (i-8) ccList)    -- check bottom
        ccList = elems cc

isTree :: Index -> [[Content]] -> Bool
isTree i cs
  | i < 0 || i >= length cs = False
  | otherwise = (cs !! i)!!0 == Tree

outCamp :: Camp -> [[Char]]
outCamp c = splitEvery 8 (map (\c -> if c == Tree then 'B' else if c == Tent then 'Z' else 'u') (elems c))

makeCamp :: LocationsOfTrees -> Camp
makeCamp ts = setTrees emptyCamp ts

setTrees :: Camp -> LocationsOfTrees -> Camp
setTrees c [] = c
setTrees c ts = c // (map (\pos-> (pos,Tree)) normTs)
  where normTs = map (\(c,r)-> (c-1,r-1)) ts

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

emptyCamp :: Camp
emptyCamp = array  ((0,0), (7,7)) [((x, y), Empty) | x <- [0..7], y <- [0..7]]

testTrees :: [(Int,Int)]
testTrees = map (\(c,r) -> (c+1,r+1)) [(0,1),(0,2),(0,6),(0,7),(1,2),(3,0),(3,4),(3,7),(4,4),(5,0),(5,3),(5,7),(6,0),(6,4),(6,6),(7,1)]

tpr :: TentsPerRow
tpr = [3,1,3,1,1,3,0,4]

tpc :: TentsPerColumn
tpc = [4,0,3,1,3,1,3,1]

camp_gen :: Array (Int,Int) Content
camp_gen = array ((0,0),(1,1)) [((0,0), Empty),((0,1), Tree),((1,0), Empty),((1,1), Tent)]

testCamp :: Array (Int, Int) Content
testCamp = array ((0,0),(2,2)) [((0,0), Empty), ((0,1), Tent), ((0,2), Empty), ((1,0), Tree), ((1,1), Empty), ((1,2), Empty), ((2,0), Empty),((2,1), Tent), ((2,2), Empty)]

optChoices :: Array (Int,Int) [Content]
optChoices = array ((0,0),(7,7)) [((0,0),[Tent,Empty]),((0,1),[Tree]),((0,2),[Tree]),((0,3),[Tent,Empty]),((0,4),[Tent,Empty]),((0,5),[Tent,Empty]),((0,6),[Tree]),((0,7),[Tree]),((1,0),[Tent,Empty]),((1,1),[Empty]),((1,2),[Tree]),((1,3),[Tent,Empty]),((1,4),[Tent,Empty]),((1,5),[Tent,Empty]),((1,6),[Tent,Empty]),((1,7),[Tent,Empty]),((2,0),[Tent,Empty]),((2,1),[Empty]),((2,2),[Tent,Empty]),((2,3),[Tent,Empty]),((2,4),[Tent,Empty]),((2,5),[Tent,Empty]),((2,6),[Tent,Empty]),((2,7),[Tent,Empty]),((3,0),[Tree]),((3,1),[Empty]),((3,2),[Tent,Empty]),((3,3),[Tent,Empty]),((3,4),[Tree]),((3,5),[Tent,Empty]),((3,6),[Tent,Empty]),((3,7),[Tree]),((4,0),[Tent,Empty]),((4,1),[Empty]),((4,2),[Tent,Empty]),((4,3),[Tent,Empty]),((4,4),[Tree]),((4,5),[Tent,Empty]),((4,6),[Tent,Empty]),((4,7),[Tent,Empty]),((5,0),[Tree]),((5,1),[Empty]),((5,2),[Tent,Empty]),((5,3),[Tree]),((5,4),[Tent,Empty]),((5,5),[Tent,Empty]),((5,6),[Tent,Empty]),((5,7),[Tree]),((6,0),[Tree]),((6,1),[Empty]),((6,2),[Empty]),((6,3),[Empty]),((6,4),[Tree]),((6,5),[Empty]),((6,6),[Tree]),((6,7),[Empty]),((7,0),[Tent,Empty]),((7,1),[Tree]),((7,2),[Tent,Empty]),((7,3),[Tent,Empty]),((7,4),[Tent,Empty]),((7,5),[Tent,Empty]),((7,6),[Tent,Empty]),((7,7),[Tent,Empty])]

optChoices2 :: Array (Int,Int) [Content]
optChoices2 = array ((0,0),(7,7)) [((0,0),[Tent]),((0,1),[Tree]),((0,2),[Tree]),((0,3),[Tent,Empty]),((0,4),[Tent,Empty]),((0,5),[Tent,Empty]),((0,6),[Tree]),((0,7),[Tree]),((1,0),[Tent,Empty]),((1,1),[Empty]),((1,2),[Tree]),((1,3),[Tent,Empty]),((1,4),[Tent,Empty]),((1,5),[Tent,Empty]),((1,6),[Tent,Empty]),((1,7),[Tent,Empty]),((2,0),[Tent,Empty]),((2,1),[Empty]),((2,2),[Tent,Empty]),((2,3),[Tent,Empty]),((2,4),[Tent,Empty]),((2,5),[Tent,Empty]),((2,6),[Tent,Empty]),((2,7),[Tent,Empty]),((3,0),[Tree]),((3,1),[Empty]),((3,2),[Tent,Empty]),((3,3),[Tent,Empty]),((3,4),[Tree]),((3,5),[Tent,Empty]),((3,6),[Tent,Empty]),((3,7),[Tree]),((4,0),[Tent,Empty]),((4,1),[Empty]),((4,2),[Tent,Empty]),((4,3),[Tent,Empty]),((4,4),[Tree]),((4,5),[Tent,Empty]),((4,6),[Tent,Empty]),((4,7),[Tent,Empty]),((5,0),[Tree]),((5,1),[Empty]),((5,2),[Tent,Empty]),((5,3),[Tree]),((5,4),[Tent,Empty]),((5,5),[Tent,Empty]),((5,6),[Tent,Empty]),((5,7),[Tree]),((6,0),[Tree]),((6,1),[Empty]),((6,2),[Empty]),((6,3),[Empty]),((6,4),[Tree]),((6,5),[Empty]),((6,6),[Tree]),((6,7),[Empty]),((7,0),[Tent,Empty]),((7,1),[Tree]),((7,2),[Tent,Empty]),((7,3),[Tent,Empty]),((7,4),[Tent,Empty]),((7,5),[Tent,Empty]),((7,6),[Tent,Empty]),((7,7),[Tent,Empty])]

tc :: Array (Int,Int) Content
tc = array ((0,0),(7,7)) [((0,0),Tent),((0,1),Tree),((0,2),Tree),((0,3),Tent),((0,4),Empty),((0,5),Tent),((0,6),Tree),((0,7),Tree),((1,0),Empty),((1,1),Empty),((1,2),Tree),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Tent),((2,0),Tent),((2,1),Empty),((2,2),Tent),((2,3),Empty),((2,4),Tent),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Tree),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Tree),((3,5),Empty),((3,6),Tent),((3,7),Tree),((4,0),Tent),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Tree),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Tree),((5,1),Empty),((5,2),Tent),((5,3),Tree),((5,4),Tent),((5,5),Empty),((5,6),Tent),((5,7),Tree),((6,0),Tree),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Tree),((6,5),Empty),((6,6),Tree),((6,7),Empty),((7,0),Tent),((7,1),Tree),((7,2),Tent),((7,3),Empty),((7,4),Tent),((7,5),Empty),((7,6),Tent),((7,7),Empty)]
