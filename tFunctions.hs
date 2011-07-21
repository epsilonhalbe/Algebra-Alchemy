import Test.HUnit
import Data
import Functions
import Ratio

main = runTestTT tests

tests = TestList (fun)

tree1 = read "((a+b)*(c-d))"::ExprTree Algebraic
tree2 = read "((a+b)/(c-d))"::ExprTree Algebraic
tree3 = read "((a+b)+c)"::ExprTree Algebraic
ltree1 = relabel tree1
ltree2 = relabel tree2
ltree3 = relabel tree3

fun = [
    TestLabel "Functions" $ TestList [
        TestLabel "Tree stuff" $ TestList [
            TestCase (is "insert L" (insert ltree1 (1%2) Add L ltree2)
                          (read "((((a+b)*(c-d))+(a+b))/(c-d))"::ExprTree Algebraic))
            ]
        ]
    ]


is ::(Show a, Eq a) => String -> a -> a -> Assertion
is = assertEqual
