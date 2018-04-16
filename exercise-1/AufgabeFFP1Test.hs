import AufgabeFFP1
import Test.HUnit
import Data.List

main = runTestTT $ TestList [test1, test2, test3, test4, test5]

test1 = TestLabel "rs_generator" $ (
    TestList [
        TestCase $ assertEqual "rs_generator --1--" (rs_generator                                                   []) [],
        TestCase $ assertEqual "rs_generator --2--" (rs_generator                                                   [(1,1)])    [[(1,1)]],
        TestCase $ assertEqual "rs_generator --3--" ((length . rs_generator) (zip [1..5] [1..5]))                   (2^5-1),
        TestCase $ assertEqual "rs_generator --4--" ((length . rs_generator) (zip [1..10] [1..10]))                 (2^10-1),
        TestCase $ assertEqual "rs_generator --5--" ((length . rs_generator) (zip [1..15] [1..15]))                 (2^15-1),
        TestCase $ assertEqual "rs_generator --6--" (sort (map (show) (rs_generator (take 2 (zip [1..] [1..])))))   ["[(1,1),(2,2)]","[(1,1)]","[(2,2)]"],
        TestCase $ assertEqual "rs_generator --7--" (sort (map (show) (rs_generator [(5,3),(2,7),(2,6)])))          ["[(2,6)]","[(2,7),(2,6)]","[(2,7)]","[(5,3),(2,6)]","[(5,3),(2,7),(2,6)]","[(5,3),(2,7)]","[(5,3)]"]
    ])

test2 = TestLabel "rs_transformer" $ (
    TestList [
        TestCase $ assertEqual "rs_transformer --1--" (rs_transformer [])                                                               [],       
        TestCase $ assertEqual "rs_transformer --2--" (rs_transformer [[]])                                                             [],
        TestCase $ assertEqual "rs_transformer --3--" (rs_transformer [take 5 (repeat (1,1))])                                          [([(1,1),(1,1),(1,1),(1,1),(1,1)],5,5)],
        TestCase $ assertEqual "rs_transformer --4--" (rs_transformer [(zip (take 5 [1..]) (take 5 [1..]))])                            [([(1,1),(2,2),(3,3),(4,4),(5,5)],15,15)],
        TestCase $ assertEqual "rs_transformer --5--" (rs_transformer [(zip (take 3 [1..]) (take 3 [1..]))])                            [([(1,1),(2,2),(3,3)],6,6)], 
        TestCase $ assertEqual "rs_transformer --6--" (rs_transformer (take 3 (repeat [(1,2)])))                                        [([(1,2)],1,2),([(1,2)],1,2),([(1,2)],1,2)],
        TestCase $ assertEqual "rs_transformer --7--" (sort (map (show) ((rs_transformer . rs_generator) [(5,3),(2,7),(2,6)])))         ["([(2,6)],2,6)","([(2,7),(2,6)],4,13)","([(2,7)],2,7)","([(5,3),(2,6)],7,9)","([(5,3),(2,7),(2,6)],9,16)","([(5,3),(2,7)],7,10)","([(5,3)],5,3)"],
        TestCase $ assertEqual "rs_transformer --8--" (sort (map (show) ((rs_transformer . rs_generator) (take 2 (zip [1..] [1..])))))  ["([(1,1),(2,2)],3,3)","([(1,1)],1,1)","([(2,2)],2,2)"]
    ])

test3 = TestLabel "rs_filter" $ (
    TestList [
        TestCase $ assertEqual "rs_filter --1--" (rs_filter 0 [])                                                                       [],
        TestCase $ assertEqual "rs_filter --2--" (((rs_filter 1) . rs_transformer) (take 3 (repeat [(1,2)])))                           [([(1,2)],1,2),([(1,2)],1,2),([(1,2)],1,2)],
        TestCase $ assertEqual "rs_filter --3--" (((rs_filter 0) . rs_transformer) (take 3 (repeat [(1,2)])))                           [],
        TestCase $ assertEqual "rs_filter --4--" (((rs_filter 1) . rs_transformer . rs_generator) (take 15 (zip [1..] [1..])))          [([(1,1)],1,1)],
        TestCase $ assertEqual "rs_filter --5--" (((rs_filter 0) . rs_transformer . rs_generator) (take 15 (zip [1..] [1..])))          [],
        TestCase $ assertEqual "rs_filter --6--" (sort (map (show) (((rs_filter 5) . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)])))    ["([(2,6)],2,6)","([(2,7),(2,6)],4,13)","([(2,7)],2,7)","([(5,3)],5,3)"],
        TestCase $ assertEqual "rs_filter --7--" (sort (map (show) (((rs_filter 3) . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)])))    ["([(2,6)],2,6)","([(2,7)],2,7)"]
    ])

test4 = TestLabel "rs_selector1/2" $ (
    TestList [
        TestCase $ assertEqual "rs_selector1/2 --1--" ((rs_selector1 . (rs_filter 1) . rs_transformer) (take 3 (repeat [(1,2)]))) [([(1,2)],1,2),([(1,2)],1,2),([(1,2)],1,2)],
        TestCase $ assertEqual "rs_selector1/2 --2--" ((rs_selector2 . (rs_filter 1) . rs_transformer) (take 3 (repeat [(1,2)]))) [([(1,2)],1,2),([(1,2)],1,2),([(1,2)],1,2)],
        TestCase $ assertEqual "rs_selector1/2 --3--" ((rs_selector1 . (rs_filter 0) . rs_transformer) (take 3 (repeat [(1,2)]))) [],
        TestCase $ assertEqual "rs_selector1/2 --4--" ((rs_selector2 . (rs_filter 0) . rs_transformer) (take 3 (repeat [(1,2)]))) [],
        TestCase $ assertEqual "rs_selector1/2 --5--" ((rs_selector1 . (rs_filter 2) . rs_transformer . rs_generator) (take 10 (zip [1..] [1..]))) [([(2,2)],2,2)],
        TestCase $ assertEqual "rs_selector1/2 --6--" ((rs_selector2 . (rs_filter 2) . rs_transformer . rs_generator) (take 10 (zip [1..] [1..]))) [([(2,2)],2,2)]
    ])

test5 = TestLabel "given Tests" $ (
    TestList [
        TestCase $ assertEqual "givenTest --1--" ((rs_selector1 . (rs_filter 5) . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)])                 [([(2,7),(2,6)],4,13)],
        TestCase $ assertEqual "givenTest --2--" ((rs_selector1 . (rs_filter 13) . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)])                [([(2,7),(10,100)],12,107)],
        TestCase $ assertEqual "givenTest --3--" ((rs_selector1 . (rs_filter 1) . rs_transformer . rs_generator) [(5,3),(2,7),(2,6),(10,100)])                 [],
        TestCase $ assertEqual "givenTest --4--" ((rs_selector2 . (rs_filter 5) . rs_transformer . rs_generator) [(5,13),(2,7),(2,6),(10,100)])                [([(2,7),(2,6)],4,13)],
        TestCase $ assertEqual "givenTest --5--" (map (show) ((rs_selector1 . (rs_filter 5) . rs_transformer . rs_generator) [(5,13),(2,7),(2,6),(10,100)]))   ["([(5,13)],5,13)","([(2,7),(2,6)],4,13)"]
    ])
