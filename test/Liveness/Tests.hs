module Livness.Tests where


import Test.HUnit
import qualified Data.Set as S

import L2.Grammar
import L2.Parser
import L2.Display
import Liveness.Liveness


main = runTestTT $ TestList [testGen, testKill]

{- Helper functions -}
parseDisplayGen = displayXList . S.toList . gen . readInstruction
genStringTest result input = TestCase $ assertEqual input result (parseDisplayGen input)

parseDisplayKill = displayXList . S.toList . kill . readInstruction
killStringTest result input = TestCase $ assertEqual input result (parseDisplayKill input)

{- gen tests -}
testGen = TestList [genStringTest "(var)" "(eax <- var)"
                   ,genStringTest "(eax)" "(var <- eax)"
                   ,genStringTest "()" "(var <- (mem ebp -4))"
                   ,genStringTest "(x)" "((mem ebp -4) <- x)"
                   ,genStringTest "(arr x)" "((mem arr -4) <- x)"
                   ,genStringTest "(abba eqx)" "((mem eqx -4) <- abba)"
                   ,genStringTest "(x y)" "(x += y)"
                   ,genStringTest "(x y)" "(x -= y)"
                   ,genStringTest "(a y)" "(y &= a)"
                   ,genStringTest "(eax tx)" "(eax += tx)"
                   ,genStringTest "()" ":fun"
                   ,genStringTest "()" "(goto :fun)"
                   ,genStringTest "()" "(goto :eax)"
                   ,genStringTest "(eax ecx edx v)" "(call v)"
                   ,genStringTest "(eax ecx edi edx esi f)" "(tail-call f)"
                   ,genStringTest "(eax edi esi)" "(return)"
                   ,genStringTest "(eax y5)" "(eax += y5)"
                   ,genStringTest "(arr)" "(eax <- (print arr))"
                   ,genStringTest "()" "(ebp -= esp)"
                   ,genStringTest "(edi esi)" "(eax <- (allocate esi edi))"
                   ,genStringTest "(edi esi)" "(eax <- (array-error esi edi))"
                   ,genStringTest "(edx)" "(eax <- (array-error edx 5))"
                   ,genStringTest "(eax tx)" "(eax += tx)"
                   ,genStringTest "(a ecx)" "(a <<= ecx)"
                   ,genStringTest "(a b)" "(q <- a < b)"
                   ,genStringTest "(f)" "(cjump f = 0 :a :b)"
                   ]


{- kill tests -}
testKill = TestList [killStringTest "()" ":f"
                    ,killStringTest "(x2)" "(x2 <- eax)"
                    ,killStringTest "(eax)" "(eax <- var)"
                    ,killStringTest "(v)" "(v <- (mem ebp -4))"
                    ,killStringTest "()" "((mem esi -4) <- x)"
                    ,killStringTest "(x2)" "(x2 *= x2)"
                    ,killStringTest "(shmuck)" "(shmuck &= shmuck)"
                    ,killStringTest "(a)" "(a <<= ecx)"
                    ,killStringTest "(b)" "(b >>= 4)"
                    ,killStringTest "(q)" "(q <- a < b)"
                    ,killStringTest "()" "(goto :fish)"
                    ,killStringTest "()" "(cjump f = 0 :a :b)"
                    ,killStringTest "(eax ebx ecx edx)" "(call :f)"
                    ,killStringTest "()" "(tail-call :g)"
                    ,killStringTest "()" "(return)"
                    ,killStringTest "(eax ecx edx)" "(eax <- (print a))"
                    ,killStringTest "(eax ecx edx)" "(eax <- (allocate a 9))"
                    ,killStringTest "(eax ecx edx)" "(eax <- (array-error a q))"
                    ]





