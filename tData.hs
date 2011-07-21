import Test.HUnit
import Data
import Ratio

main = runTestTT tests

tests = TestList (tFun ++ tAlgebraic ++ tExprTree)

tFun = [
    TestLabel "tFun" $ TestList [
        TestLabel "read" $ TestList [
            TestCase (is "Add" (read "+"::Fun) Add),
            TestCase (is "Sub" (read "-"::Fun) Sub),
            TestCase (is "Mul" (read "*"::Fun) Mul),
            TestCase (is "Div" (read "/"::Fun) Div)
            ],
        TestLabel "show" $ TestList [
            TestCase (is "Add" (show Add) "+"),
            TestCase (is "Sub" (show Sub) "-"),
            TestCase (is "Mul" (show Mul) "*"),
            TestCase (is "Div" (show Div) "/")
            ]
        ]
    ]

tAlgebraic = [
    TestLabel "tAlgebraic" $ TestList [
        TestLabel "read" $ TestList [
            TestCase (is "x1" (read "x1"::Algebraic) (Alg "x1")),
            TestCase (is "1%2" (read "1%2"::Algebraic) (Alg "1%2"))
            ],
        TestLabel "show" $ TestList [
            TestCase (is "x1" (show (Alg "x1")) "_x1_"),
            TestCase (is "1%2" (show (Alg "1%2")) "_1%2_")
            ],
        TestLabel "eval" $ TestList [
            TestCase (is "1%2" (eval (Alg "1%2")) (1%2::Rational)),
            TestCase (is "1" (eval (Alg "1")) (1%1::Rational))
            ]
        ]
    ]

tExprTree = [
    TestLabel "tExprTree" $ TestList [
        TestLabel "read" $ TestList [
 {-           TestCase (is "((((1 + 2) - 3) * 4) / 5)" (read "((((1 + 2) - 3) * 4) / 5)"::(ExprTree Algebraic))
                (Node (0%1)
                    Div (Node (0%1)
                        Mul (Node (0%1)
                            Sub (Node (0%1)
                                Add (Leaf (0%1) (Alg "1"))
                                    (Leaf (0%1) (Alg "2"))
                            (Leaf (0%1) (Alg "3")))
                        (Leaf (0%1) (Alg "4")))
                    (Leaf (0%1) (Alg "5"))))) -}
                TestCase (is "1" (read "1"::ExprTree Algebraic) (Leaf (1%0) (Alg "1")))
            ]
        ]
    ]


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
