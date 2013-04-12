module L2.Parser.Tests where

import Test.HUnit

import L2.Grammar
import L2.Parser


main = runTestTT $ TestList [testEmpty, testAssign]

testEmpty = TestCase $ assertEqual "Should get empty L2Program from empty string"
                                   (L2Program [] [])
                                   (readProg "")

testAssign = TestCase $ do
    assertEqual ""
                (L2Assign (L2Xvar (L2Var "a")) (L2SX (L2Xvar (L2Var "b"))))
                (readInstruction "(a <- b)")
    assertEqual "" 
                (L2Assign (L2Xreg L2ESI) (L2SX (L2Xvar (L2Var "eaxa"))))
                (readInstruction "(esi <- eaxa)")
