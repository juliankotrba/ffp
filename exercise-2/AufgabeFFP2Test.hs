import AufgabeFFP2
import Test.HUnit
import Data.List

main = runTestTT $ TestList [test1, test2, test3, test4]

test1 = TestLabel "succ_ks" $ (
    TestList [
        TestCase $ assertEqual "succ_ks _,_,_,[],_" (succ_ks (0,0,0,[],[])) ([]),
        TestCase $ assertEqual "succ_ks _,_,0,[(2,3)],_" (succ_ks (0,0,0,[(2,3)],[])) ([]),
        TestCase $ assertEqual "succ_ks (0,0,5,[(2,3)],[])" (succ_ks (0,0,5,[(2,3)],[])) ([(3,2,5,[],[(2,3)])]),
        TestCase $ assertEqual "succ_ks case --4--" (succ_ks (0,0,1,take 10 (repeat (1,1)),[])) (take 10 (repeat (1,1,1,take 9 (repeat (1,1)),[(1,1)]))),
        TestCase $ assertEqual "succ_ks case --5--" (succ_ks (head (succ_ks (0,0,2,take 5 (repeat (1,1)),[])))) (take 4 (repeat (2,2,2,take 3 (repeat (1,1)),take 2 (repeat (1,1))))),
        TestCase $ assertEqual "succ_ks case --6--" (succ_ks (head (succ_ks (head (succ_ks (0,0,3,take 5 (repeat (1,1)),[])))))) (take 3 (repeat (3,3,3,take 2 (repeat (1,1)),take 3 (repeat (1,1)))))           
    ])

test2 = TestLabel "goal_ks" $ (
    TestList [
        TestCase $ assertEqual "goal_ks case --1--" (goal_ks (head (succ_ks (0,0,1,take 3 (repeat (1,1)),[])))) True,
        TestCase $ assertEqual "goal_ks case --2--" (goal_ks (head (succ_ks (0,0,2,take 3 (repeat (1,1)),[])))) False,
        TestCase $ assertEqual "goal_ks case --3--" (goal_ks (0,0,0,[],[])) True,
        TestCase $ assertEqual "goal_ks case --4--" (goal_ks (0,0,0,[(2,3)],[])) True,
        TestCase $ assertEqual "goal_ks case --5--" (goal_ks (0,0,3,[(2,3)],[])) False           
    ])

test3 = TestLabel "knapsack" $ (
    TestList [
        TestCase $ assertEqual "given TestCase"             (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)               ([(2,3),(2,3),(3,4),(3,4)],14),    
        TestCase $ assertEqual "knapsack [] _ == ([],0)"    (knapsack [] 10)                                            ([],0),
        TestCase $ assertEqual "knapsack xs 0 == ([],0)"    (knapsack [(2,3),(2,3),(3,4),(3,4),(5,6)] 10)               ([(2,3),(2,3),(3,4),(3,4)],14),    
        TestCase $ assertEqual "knapsack case --4--"        (knapsack [(7,9), (6,8), (5,8), (1,1), (8,10), (5,2)] 6)    ([],0),
        TestCase $ assertEqual "knapsack case --5--"        (knapsack [(5,3),(2,7),(2,6),(10,100)] 5)                   ([(2,7)], 7),    
        TestCase $ assertEqual "knapsack case --6--"        (knapsack [(5,3),(2,7),(2,6),(10,100)] 13)                  ([(10,100),(2,7)], 107),
        TestCase $ assertEqual "knapsack case --7--"        (knapsack [(5,3),(2,7),(2,6),(10,100)] 5)                   ([(2,7)],7),
        TestCase $ assertEqual "knapsack case --8--"        (knapsack [(5,3),(2,7),(2,6),(10,100)] 13)                  ([(10,100),(2,7)],107),
        TestCase $ assertEqual "knapsack case --9--"        (knapsack [(5,3),(2,7),(2,6),(10,100)] 1)                   ([],0),
        TestCase $ assertEqual "knapsack case --10--"       (knapsack [(5,13),(2,7),(2,6),(10,100)] 5)                  ([(5,13)],13)
    ])

test4 = TestLabel "binom_dp" $ (
    TestList [
        TestCase $ assertEqual "binom_dp _ 0 == 1"           (binom_dp(10,0))     1,        
        TestCase $ assertEqual "binom_dp n n == 1"           (binom_dp(10,10))    1,
        TestCase $ assertEqual "binom_dp n n+1 == 0"         (binom_dp(10,11))    0,
        TestCase $ assertEqual "binom_dp _ -1 == 0"          (binom_dp(10,(-1)))  0,
        TestCase $ assertEqual "binom_dp n k == binom n k"   (binom_dp(10,5))     252,
        TestCase $ assertEqual "binom_dp 100 50"             (binom_dp(100,50))   100891344545564193334812497256 
    ])
