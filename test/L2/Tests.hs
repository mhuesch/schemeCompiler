module L2.Tests where

import Test.HUnit

import L2.Grammar
import L2.Parser
import L2.Display


main = runTestTT $ TestList testsList


{- List of tests to be run -}
testsList = testEmpty:testAssign:((map identityInstr instructionList) ++ (map identityInstrList instrListList))


{- Parse specific tests -}
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
    assertEqual ""
                (L2Assign (L2Xvar (L2Var "-9a")) (L2Snum 9))
                (readInstruction "(-9a <- 9)")
    assertEqual ""
                (L2Assign (L2Xvar (L2Var "-9a")) (L2SX (L2Xvar (L2Var "-9b"))))
                (readInstruction "(-9a <- -9b)")
    assertEqual ""
                (L2Arith (L2Xvar (L2Var "__1a")) L2Sub (L2TX (L2Xvar (L2Var "_7"))))
                (readInstruction "(__1a -= _7)")



{- Parse & Display identity instruction tests -}
identityInstr s = TestCase $ assertEqual ""
                                         s
                                         (displayInstruction . readInstruction $ s)

instructionList = [ "(eax <- x)"
                  , "(eax <- y)"
                  , "(eaxa <- z)"
                  , "(y <- ebx)"
                  , "(z <- ebx_)"
                  , "(ecx1 <- 99)"
                  , "(eax <- :lol)"
                  , "(-9a <- 9)"
                  , "(-a <- 5)"
                  , "(-a <- -b)"
                  , "(-9a <- -9b)"
                  , "(x <- (mem ebp 8))"
                  , "(y <- (mem cat 12))"
                  , "(eax <- (mem eax -4))"
                  , "(esi <- (mem ebp -12))"
                  , "((mem ebp -12) <- esi)"
                  , "((mem x -96) <- y)"
                  , "((mem esi_ -4) <- ebp__1)"
                  , "((mem ebp -4) <- -a)"
                  , "((mem eax 44) <- -11990a)"
                  , "(eax += 1)"
                  , "(eax_ += eaxb)"
                  , "(-91a -= -c121)"
                  , "(x *= esi)"
                  , "(a0 &= 5)"
                  , "(a0 &= a5)"
                  , "(__1a -= _7)"
                  , "(__1a <<= ecx)"
                  , "(-a >>= ecx)"
                  , "(x <<= 4)"
                  , "(ecx >>= 99)"
                  , "(ecx <- 9 < 99)"
                  , "(eax <- 9 = x)"
                  , "(edx <- x < y)"
                  , "(sdfSDF <- -AA90 <= -999zzzzz)"
                  , ":laberrr"
                  , ":a"
                  , ":_99999999"
                  , "(goto :aaaa)"
                  , "(goto :_00)"
                  , "(goto :ZZ99aa)"
                  , "(cjump 9 = 0 :a :b)"
                  , "(cjump eax < eax_a :_0 :_asdf9)"
                  , "(cjump -9a <= -c0 :firST :secOND)"
                  , "(call :f)"
                  , "(call eax)"
                  , "(call -a9)"
                  , "(call esix)"
                  , "(call -9a)"
                  , "(tail-call :g)"
                  , "(tail-call :eax)"
                  , "(tail-call esi)"
                  , "(tail-call -9a)"
                  , "(tail-call edix)"
                  , "(return)"
                  , "(eax <- (print -1val))"
                  , "(eax <- (print eax))"
                  , "(eax <- (print esix))"
                  , "(eax <- (print 999))"
                  , "(eax <- (print -11))"
                  , "(eax <- (allocate 9 99))"
                  , "(eax <- (allocate esi ebx))"
                  , "(eax <- (allocate esix ebxx))"
                  , "(eax <- (allocate esi_ -78))"
                  , "(eax <- (array-error -8 -12))"
                  , "(eax <- (array-error esi ex))"
                  , "(eax <- (array-error ebp -72))"
                  , "(eax <- (array-error x y))"
                  ]


{- Parse & Display identity instruction tests -}
identityInstrList s = TestCase $ assertEqual ""
                                             s
                                             (displayInstrList . readInstrList $ s)

instrListList = [ "(:main\n(x <- 51)\n(y <- 9)\n(eax <- (allocate x y))\n(a1 <- eax)\n(eax <- (array-error a1 99))\n(eax <- (allocate y x))\n(a2 <- eax)\n(eax <- (array-error a2 x)))"
                ]

