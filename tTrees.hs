import Data
import Functions

import TAP

main :: IO ()
main = runTests $ do
            planTests = 1

            is ( makeTree 3 [1..7] ) (Node 1%1 1 ( Node 1%2 (Leaf 1%4 4) (Leaf 3%4 5))
                                                 ( Node 3%2 3 (Leaf 5%4 6) (Leaf 7%4 7)))
                                                    $ Just "growing a tree"
