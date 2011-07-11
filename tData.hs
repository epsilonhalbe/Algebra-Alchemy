import Test.HUnit

main = runTestTT tests

tests = TestList [TestLabel "failure1" test1,
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
