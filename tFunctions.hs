import Test.HUnit
import Data
import Functions
import Ratio

main = runTestTT tests

tests = TestList (tree ++ alg)

tree1 = read "((a+b)*(c-d))"::ExprTree Algebraic
tree2 = read "((a+b)/(c-d))"::ExprTree Algebraic
tree3 = read "((a+b)+c)"::ExprTree Algebraic
tree4 = read "((a+b)+1%2)"::ExprTree Algebraic
ltree1 = relabel tree1
ltree2 = relabel tree2
ltree3 = relabel tree3
srules = [(Alg "a",1%1),(Alg "b",2%1),(Alg "c",3%1),(Alg "d",4%1)]

tree = [
    TestLabel "Tree" $ TestList [
        TestLabel "insert" $ TestList [
            TestCase (is "insert L" (insert ltree1 (1%2) Add L ltree2)
                          (read "((((a+b)*(c-d))+(a+b))/(c-d))"::ExprTree Algebraic)),
            TestCase (is "insert R" (insert ltree1 (3%2) Add R ltree2)
                          (relabel $ read "((a+b)/((c-d)+((a+b)*(c-d))))"::ExprTree Algebraic))
            ],
        TestLabel "substitute" $ TestList [
            TestCase (is "tree1" (substitute srules tree1)
                                 (read "((1%1+2%1)*(3%1-4%1))"::ExprTree Rational)),
            TestCase (is "(a+b)" (substitute srules (read "(a+b)"::ExprTree Algebraic))
                                 (read "(1%1+2%1)"::ExprTree Rational))
            ],
        TestLabel "forkWith" $ TestList [
            TestCase (is "tree1 /*\\ tree2" (forkWith tree1 Mul L tree2)
                                 (read "(((a+b)*(c-d))*((a+b)/(c-d)))"::ExprTree Algebraic))
            ],
        TestLabel "commutate" $ TestList [
            TestCase (is "commutate"  (commutate tree1)
                                      (read "((c-d)*(a+b))"::ExprTree Algebraic))
            ]
        ]
    ]
alg = [
    TestLabel "Algebraic" $ TestList [
        TestLabel "_substitute" $ TestList [
            TestCase (is "_substiute a" (_substitute srules (Alg "a")) (1%1)),
            TestCase (is "_substiute 1%2" (_substitute srules (Alg "1%2")) (1%2)),
            TestCase (is "_substiute b" (_substitute srules (Alg "b")) (2%1)),
            TestCase (is "_substiute c" (_substitute srules (Alg "c")) (3%1))
            ],
        TestLabel "($$)" $ TestList [
            TestCase (is "(f1,f2)(1,2)" (((+2),(==3))$$(1,2)) (3,False))
            ],
        TestLabel "diag" $ TestList [
            TestCase (is "1 -> (1,1)" (diag 1) (1,1))
            ]
        ]
    ]


is ::(Show a, Eq a) => String -> a -> a -> Assertion
is = assertEqual
