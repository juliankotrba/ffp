-- author: schogglomat
-- https://www.informatik-forum.at/showthread.php?115947-Tests-Exercise-3

import AufgabeFFP3
import Test.HUnit
import Data.List
import Data.Array

main = runTestTT $ TestList [test1]

test1 = TestLabel "Schach" $
    TestList [
        makeTest 1    [ "S-------" -- test 0
                      , "--------"
                      , "--------"
                      , "--------"
                      , "F-------"
                      , "--------"
                      , "--------"
                      , "--------"] "one move"
        , makeTest 2  [ "S-------" -- test 1
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "-------F"] "two moves"
        , makeTest 3  [ "S--B----" -- test 2
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "B------F"] "three moves"
        , makeTest 4  [ "SB------" -- test 3
                      , "-----B--"
                      , "BBBB----"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "-------F"] "four moves"
        , makeTest 5  [ "SB------" -- test 4
                      , "----B---"
                      , "BBBB----"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "-------F"] "five moves"
        , makeTest 0  [ "S--B----" -- test 5
                      , "BBBB----"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "-------F"] "locked in"
        , makeTest' 0 [ "--------" -- test 6
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"] "start is finish" (D,V) (D,V)
        , makeTest' 0 [ "--------" -- test 7
                      , "--------"
                      , "--------"
                      , "--------"
                      , "---B----"
                      , "--------"
                      , "--------"
                      , "--------"] "start is blocked" (D,IV) (A,VI)
        , makeTest' 0 [ "--------" -- test 8
                      , "--------"
                      , "--------"
                      , "--------"
                      , "---B----"
                      , "--------"
                      , "--------"
                      , "--------"] "finish is blocked" (A,VI) (D,IV)
        , makeTest 3  [ "S------F" -- test 9
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"
                      , "--------"] "needs unnecessary moves"
        , makeTest 0  [ "SB------" -- test 10
                      , "----B---"
                      , "BBBB----"
                      , "--------"
                      , "--------"
                      , "F-------"
                      , "-----BBB"
                      , "--------"] "needs 6 moves"
        ]

makeTest :: Int -> [String] -> String -> Test
makeTest numberOfMoves field testName = TestCase (assertValidResult testName board task result)
    where
        board = convertField field
        task = (charPos 'S', charPos 'F', toEnum numberOfMoves)
        result = suche board task
        charPos c = head $ map (\(x, _) -> (toEnum (x `mod` 8), toEnum (x `div` 8))) $ filter ((==) c . snd) $ zip [0..] (concat $ reverse field)
        

makeTest' :: Int -> [String] -> String -> Startfeld -> Zielfeld -> Test
makeTest' numberOfMoves field testName start finish = TestCase (assertValidResult testName board task result)
    where
        board = convertField field
        task = (start, finish, toEnum numberOfMoves)
        result = suche board task
        charPos c = head $ map (\(x, _) -> (toEnum (x `mod` 8), toEnum (x `div` 8))) $ filter ((==) c . snd) $ zip [0..] (concat $ reverse field)
            
convertField :: [String] -> Schachbrett
convertField rows = listArray ((A,I), (H,VIII)) (map ('B' ==) (concat $ transpose $ reverse rows))

assertValidResult :: String -> Schachbrett -> Aufgabe -> Zugfolge -> IO ()
assertValidResult testName board (start, finish, numberOfMoves) moves
    | null moves && numberOfMoves == Null = pure ()
    | length moves /= fromEnum numberOfMoves = assertFailure (testName ++ ": Expected " ++ show (fromEnum numberOfMoves) ++ " moves but result has " ++ show (length moves))
    | fst (head moves) /= start = assertFailure (testName ++ ": Expected start field " ++ show start ++ " but result has " ++ show (fst (head moves)))
    | snd (last moves) /= finish = assertFailure (testName ++ ": Expected finish field " ++ show finish ++ " but result has " ++ show (snd (last moves)))
    | otherwise = case map snd $ filter fst $ map (\(a,b) -> (snd a /= fst b, (a,b))) (zip moves (tail moves)) of
        [] -> case invalidMoves of
            [] -> pure ()
            (x:_) -> assertFailure (testName ++ ": Invalid move " ++ show x ++ " in moves " ++ show moves)
        (x:_) -> assertFailure (testName ++ ": Invalid move sequence " ++ show x ++ " in moves " ++ show moves)
    where
        invalidMoves = map snd $ filter fst $ map checkMove moves
        checkMove move@(a@(r,z), b@(r',z'))
            | r /= r' && z /= z' = (True, move)
            | otherwise = (not $ allFree a b, move)
        allFree (r,z) (r',z')
            | r == r' = and [not $ board ! (r, z'') | z'' <- makeRange z z']
            | z == z' = and [not $ board ! (r'', z) | r'' <- makeRange r r']
            | otherwise = error "Cannot check if all fields are free"
        makeRange a b
            | a < b = enumFromTo a b
            | otherwise = enumFromTo b a
