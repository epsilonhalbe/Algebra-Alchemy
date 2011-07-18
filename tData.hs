import Test.HUnit
import Data

main = runTestTT tests

tests = TestList (tFun ++ tAlgebraic ++ tExprTree)

tFun = [
    TestLabel "read" $ TestList [
        TestCase (is "Add" (read "+"::Fun) Add),
        TestCase (is "Sub" (read "-"::Fun) Sub),
        TestCase (is "Mul" (read "*"::Fun) Mul),
        TestCase (is "Div" (read "/"::Fun) Div)
        ],
        -- TestCase (assertFailure (show Pow))
    TestLabel "show" $ TestList [
        TestCase (is "Add" (show Add) "+"),
        TestCase (is "Sub" (show Sub) "-"),
        TestCase (is "Mul" (show Mul) "*"),
        TestCase (is "Div" (show Div) "/")
        ]
    ]

tFun2 = TestCase (is "show Add" (show Add) "+")
tFun1 = TestCase (is "show Add" (show Add) "+")

tAlgebraic = []

tExprTree = []


is ::(Show a, Eq a) => String -> a -> a -> Assertion
is = assertEqual

tests1 = TestList [TestLabel "failure1" test1,
                  TestLabel "message gets displayed in case of failure" test2,
                  TestLabel "failure3" test3,
                  TestLabel "failure4" test4]

test1 = TestCase (assertEqual "message gets displayed in case of error" 3 3)
test2 = TestCase (assertEqual "msg2-error" (head (foo 1)) 1)
test3 = TestCase (assertEqual "msg3-error" 2 3)
test4 = TestCase (assertEqual "msg4-error" True False)

foo :: Int -> [Int]
foo 1 = []
foo 2 = [1]
foo _ = [1..10]
