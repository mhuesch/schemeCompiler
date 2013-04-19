module Livness.Tests where


import Test.HUnit
import qualified Data.Set as S

import L2.Grammar
import L2.Parser
import L2.Display
import Liveness.Liveness


main = runTestTT testGen

{- Helper functions -}
parseDisplayGen input = displayXList . S.toList . gen . readInstruction $ input


{- Gen tests -}
stringStringTest result input = TestCase $ assertEqual input result (parseDisplayGen input)

testGen = TestList [stringStringTest "(var)" "(eax <- var)"
                   ,stringStringTest "(eax)" "(var <- eax)"
                   ,stringStringTest "()" "(var <- (mem ebp -4))"
                   ,stringStringTest "(x)" "((mem ebp -4) <- x)"
                   ,stringStringTest "(arr x)" "((mem arr -4) <- x)"
                   ,stringStringTest "(abba eqx)" "((mem eqx -4) <- abba)"
                   ,stringStringTest "(x y)" "(x += y)"
                   ,stringStringTest "(x y)" "(x -= y)"
                   ,stringStringTest "(a y)" "(y &= a)"
                   ,stringStringTest "(eax tx)" "(eax += tx)"
                   ,stringStringTest "()" ":fun"
                   ,stringStringTest "()" "(goto :fun)"
                   ,stringStringTest "()" "(goto :eax)"
                   ,stringStringTest "(eax ecx edx v)" "(call v)"
                   ,stringStringTest "(eax ecx edi edx esi f)" "(tail-call f)"
                   ,stringStringTest "(eax edi esi)" "(return)"
                   ,stringStringTest "(eax y5)" "(eax += y5)"
                   ,stringStringTest "(arr)" "(eax <- (print arr))"
                   ,stringStringTest "()" "(ebp -= esp)"
                   ,stringStringTest "(edi esi)" "(eax <- (allocate esi edi))"
                   ,stringStringTest "(edi esi)" "(eax <- (array-error esi edi))"
                   ,stringStringTest "(edx)" "(eax <- (array-error edx 5))"
                   ]
