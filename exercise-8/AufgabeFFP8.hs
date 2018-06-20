-- Julian Kotrba
-- 01427123

module AufgabeFFP8 where

import Data.Array
import Data.List

data Content = Tree | Tent | Empty deriving (Eq,Ord,Show)

type Camp = Array (Int,Int) Content
type ChoicesCamp = Array (Int,Int) Choices

type Row = Int
type Column = Int
type LocationsOfTrees = [(Row,Column)]

type TentsPerRow = [Int]
type TentsPerColumn = [Int]

type Choices = [Content]
type Grid = [[Choices]]
type Index = Int

test1 = outCamp $ head $ filter (\x-> valid x testTpr testTpc) $ expand $ removeTreelessTents $ (choices $ (makeCamp testTrees))
test2 = outCamp $ head $ filter (\x-> valid x testTpr testTpc) $ search (choices $ (makeCamp testTrees)) testTpr testTpc

--http://www.raetsel-witte.de/produkte/zeitungsverlage/printraetsel/logikraetsel-seite-3/
test3 = outCamp $ smartCamp trees tpr tpc
  where trees = [(1,2),(2,1),(2,5),(2,6),(3,3),(3,7),(4,1),(4,6),(6,5),(6,8),(8,1),(8,6),(8,7)]
        tpr = [3,1,1,2,1,2,0,3]
        tpc = [2,2,1,1,3,0,3,1]

simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp ts tpr tpc = head $ filter (\x-> valid x tpr tpc) $ expand $ removeTreelessTents $ choices $ makeCamp ts

smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp ts tr tc = head $ filter (\x-> valid x tr tc) $ search (choices $ (makeCamp ts)) tr tc

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
valid c tpr tpc = (map countTent (rows c)) == tpr &&
                  (map countTent (cols c)) == tpc &&
                  (isTentPerTree c) &&
                  (not (hasTentContact c))

countTent :: [Content] -> Int
countTent cs = length (filter (\c -> c == Tent) cs)

cols :: Camp -> [[Content]]
cols c = [[c ! (j, i) | j<-[0..7] ] | i <- [0..7]]

rows :: Camp -> [[Content]]
rows c = [[c ! (i, j) | j<-[0..7] ] | i <- [0..7]]

isTentPerTree :: Camp -> Bool
isTentPerTree c = all (\(l,c) -> if c == Tree then (hasTent l) else True) l
  where l = zip [(x,y) | x <- [0..7], y<-[0..7]] (elems c)
        hasTent (x,y) = (isTent (x-1,y) c) || -- check top
                        (isTent (x,y-1) c) || -- check left
                        (isTent (x+1,y) c) || -- check bottom
                        (isTent (x,y+1) c)    -- check right

hasTentContact :: Camp -> Bool
hasTentContact c = any (\(l,c) -> if c == Tent then (hasTent l) else False) l
  where l = zip [(x,y) | x <- [0..7], y<-[0..7]] (elems c)
        hasTent (x,y) = (isTent (x-1,y) c)    ||  -- check top
                        (isTent (x,y-1) c)    ||  -- check left
                        (isTent (x+1,y) c)    ||  -- check bottom
                        (isTent (x,y+1) c)    ||  -- check right
                        (isTent (x+1,y+1) c)  ||  -- check bottom right corner
                        (isTent (x-1,y-1) c)  ||  -- check top left corner
                        (isTent (x+1,y-1) c)  ||  -- check bottom left corner
                        (isTent (x-1,y+1) c)      -- check top right corner

isTent :: (Index,Index) -> Camp -> Bool
isTent (x,y) c
  | x<0 || y<0 = False
  | x>=8 || y>=8 = False
  | otherwise =  c!(x,y) == Tent

removeTreelessTents :: ChoicesCamp -> ChoicesCamp
removeTreelessTents cc = listArray ((0,0),(7,7)) ((map (\(i,cs) -> if (length cs == 1) || (hasTent i) then cs else [Empty]) (zip [0..length ccList] ccList)))
  where hasTent i = (isTree (i-1) ccList) || -- check left
                    (isTree (i+1) ccList) || -- check right
                    (isTree (i+8) ccList) || -- check top
                    (isTree (i-8) ccList)    -- check bottom
        ccList = elems cc

removeZeroRowChoices :: ChoicesCamp -> TentsPerRow -> ChoicesCamp
removeZeroRowChoices cc tpc = cc//[((ci,i), [Empty]) | i <- [0..7]]
  where zeroIndexes = map (\(i,c) -> i) (filter (\(_,c) -> c == 0) (zip [0..7] tpc))
        ci = tpc !! 0

isTree :: Index -> [[Content]] -> Bool
isTree i cs
  | i < 0 || i >= length cs = False
  | otherwise = (cs !! i)!!0 == Tree


{- Optimized solution -}

search :: ChoicesCamp -> TentsPerRow -> TentsPerColumn -> [Camp]
search m tr tc
 | not (safe (cc2ps m) tr tc) = []
 | complete m0 = [ (fmap (\(x:[]) -> x)) m0]
 | otherwise = concat (map (\x-> search x tr tc) (expand1 m0))
  where m0 = removeTreelessTents m -- TODO: prune all invalid choices

safe :: Camp -> TentsPerRow -> TentsPerColumn -> Bool
safe c tpr tpc = all (\(x,y)-> x<=y) (zip (map countTent (rows c)) tpr) &&
                 all (\(x,y)-> x<=y)  (zip (map countTent (cols c)) tpc)  &&
                 (not (hasTentContact c))

complete :: ChoicesCamp -> Bool
complete cc = all (\x -> x==True) $ elems $ (fmap (\x-> if length x == 1 then True else False) cc)

-- creates a pseudo camp for safe check
-- single tents convert to Tent, everthing else to Empty
-- this allows us to check if camps have contact or if there are to many campts in a row/column
cc2ps :: ChoicesCamp -> Camp
cc2ps cc = fmap (\x -> if x==[Tree] then Tree else if x==[Tent] then Tent else Empty) cc

expand1 :: ChoicesCamp -> [ChoicesCamp]
expand1 cc = map (\x->listArray ((0,0),(7,7)) (concat x)) ([rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c<-cs])
  where
    (rows1,row:rows2) = AufgabeFFP8.break (any smallest) rows
    (row1, cs:row2) =  AufgabeFFP8.break smallest row
    smallest cs = length cs == n
    n = minimum (counts rows)
    counts = filter (/=1) . map length . concat
    rows = splitEvery 8 $ (elems cc)

break p xs
 = (takeWhile (not . p) xs, dropWhile (not . p) xs)


outCamp :: Camp -> [[Char]]
outCamp c = splitEvery 8 (map (\c -> if c == Tree then 'B' else if c == Tent then 'Z' else 'u') (elems c))

makeCamp :: LocationsOfTrees -> Camp
makeCamp ts = setTrees emptyCamp ts

emptyCamp :: Camp
emptyCamp = array  ((0,0), (7,7)) [((x, y), Empty) | x <- [0..7], y <- [0..7]]

setTrees :: Camp -> LocationsOfTrees -> Camp
setTrees c [] = c
setTrees c ts = c // (map (\pos-> (pos,Tree)) normTs)
  where normTs = map (\(c,r)-> (c-1,r-1)) ts

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where (first,rest) = splitAt n list

-- REMOVE LATER

testTrees :: [(Int,Int)]
testTrees = map (\(c,r) -> (c+1,r+1)) [(0,1),(0,2),(0,6),(0,7),(1,2),(3,0),(3,4),(3,7),(4,4),(5,0),(5,3),(5,7),(6,0),(6,4),(6,6),(7,1)]

testTpr :: TentsPerRow
testTpr = [3,1,3,1,1,3,0,4]

testTpc :: TentsPerColumn
testTpc = [4,0,3,1,3,1,3,1]

optChoices :: Array (Int,Int) [Content]
optChoices = array ((0,0),(7,7)) [((0,0),[Tent,Empty]),((0,1),[Tree]),((0,2),[Tree]),((0,3),[Tent,Empty]),((0,4),[Tent,Empty]),((0,5),[Tent,Empty]),((0,6),[Tree]),((0,7),[Tree]),((1,0),[Tent,Empty]),((1,1),[Empty]),((1,2),[Tree]),((1,3),[Tent,Empty]),((1,4),[Tent,Empty]),((1,5),[Tent,Empty]),((1,6),[Tent,Empty]),((1,7),[Tent,Empty]),((2,0),[Tent,Empty]),((2,1),[Empty]),((2,2),[Tent,Empty]),((2,3),[Tent,Empty]),((2,4),[Tent,Empty]),((2,5),[Tent,Empty]),((2,6),[Tent,Empty]),((2,7),[Tent,Empty]),((3,0),[Tree]),((3,1),[Empty]),((3,2),[Tent,Empty]),((3,3),[Tent,Empty]),((3,4),[Tree]),((3,5),[Tent,Empty]),((3,6),[Tent,Empty]),((3,7),[Tree]),((4,0),[Tent,Empty]),((4,1),[Empty]),((4,2),[Tent,Empty]),((4,3),[Tent,Empty]),((4,4),[Tree]),((4,5),[Tent,Empty]),((4,6),[Tent,Empty]),((4,7),[Tent,Empty]),((5,0),[Tree]),((5,1),[Empty]),((5,2),[Tent,Empty]),((5,3),[Tree]),((5,4),[Tent,Empty]),((5,5),[Tent,Empty]),((5,6),[Tent,Empty]),((5,7),[Tree]),((6,0),[Tree]),((6,1),[Empty]),((6,2),[Empty]),((6,3),[Empty]),((6,4),[Tree]),((6,5),[Empty]),((6,6),[Tree]),((6,7),[Empty]),((7,0),[Tent,Empty]),((7,1),[Tree]),((7,2),[Tent,Empty]),((7,3),[Tent,Empty]),((7,4),[Tent,Empty]),((7,5),[Tent,Empty]),((7,6),[Tent,Empty]),((7,7),[Tent,Empty])]

tc :: Array (Int,Int) Content
tc = array ((0,0),(7,7)) [((0,0),Tent),((0,1),Tree),((0,2),Tree),((0,3),Tent),((0,4),Empty),((0,5),Tent),((0,6),Tree),((0,7),Tree),((1,0),Empty),((1,1),Empty),((1,2),Tree),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Tent),((2,0),Tent),((2,1),Empty),((2,2),Tent),((2,3),Empty),((2,4),Tent),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Tree),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Tree),((3,5),Empty),((3,6),Tent),((3,7),Tree),((4,0),Tent),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Tree),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Tree),((5,1),Empty),((5,2),Tent),((5,3),Tree),((5,4),Tent),((5,5),Empty),((5,6),Tent),((5,7),Tree),((6,0),Tree),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Tree),((6,5),Empty),((6,6),Tree),((6,7),Empty),((7,0),Tent),((7,1),Tree),((7,2),Tent),((7,3),Empty),((7,4),Tent),((7,5),Empty),((7,6),Tent),((7,7),Empty)]
