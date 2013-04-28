module Graph.GraphGlueTest where


import Test.HUnit

import Graph.GraphGlue


main = runTestTT $ TestList [testRemoveSym]


rsHelp input result = TestCase $ assertEqual "" (removeSym input) result

testRemoveSym = TestList [rsHelp [(1,2),(2,1)] [(1,2)]
                         ,rsHelp [(1,2),(2,1),(32,0)] [(1,2),(32,0)]
                         ,rsHelp [(2,1),(1,2),(1,2)] [(2,1)]
                         ]