module Liveness.Tests where


import Test.HUnit
import qualified Data.Set as S
import qualified Data.Map as M

import L2.AbsL
import L2.LexL2
import L2.ParL2
import L2.PrintL
import L2.ErrM
import Liveness.Liveness


main ::  IO Counts
main = runTestTT $ TestList [testGen, testKill, testFOI, testLOI]

unsafeParse ::  Err t -> t
unsafeParse (Bad msg) = error msg
unsafeParse (Ok res) = res

{- gen tests -}
parsrdisplayGen = printSortedList . S.toList . gen . unsafeParse . pInstruction . myLexer
genStringTest result input = TestCase $ assertEqual input result (parsrdisplayGen input)

testGen = TestList [genStringTest "(var)" "(rax <- var)"
                   ,genStringTest "(rax)" "(var <- rax)"
                   ,genStringTest "(rbp)" "(var <- (mem rbp -4))"
                   ,genStringTest "(rbp x)" "((mem rbp -4) <- x)"
                   ,genStringTest "(arr x)" "((mem arr -4) <- x)"
                   ,genStringTest "(abba eqx)" "((mem eqx -4) <- abba)"
                   ,genStringTest "(x y)" "(x += y)"
                   ,genStringTest "(x y)" "(x -= y)"
                   ,genStringTest "(a y)" "(y &= a)"
                   ,genStringTest "(rax tx)" "(rax += tx)"
                   ,genStringTest "()" ":fun"
                   ,genStringTest "()" "(goto :fun)"
                   ,genStringTest "()" "(goto :rax)"
                   ,genStringTest "(r8 r9 rcx rdi rdx rsi v)" "(call v)"
                   ,genStringTest "(f r12 r13 r14 r15 r8 r9 rbp rbx rcx rdi rdx rsi)" "(tail-call f)"
                   ,genStringTest "(r12 r13 r14 r15 rax rbp rbx)" "(return)"
                   ,genStringTest "(rax y5)" "(rax += y5)"
                   ,genStringTest "(arr)" "(rax <- (print arr))"
                   ,genStringTest "(rbp)" "(rbp -= rsp)"
                   ,genStringTest "(rdi rsi)" "(rax <- (allocate rsi rdi))"
                   ,genStringTest "(rdi rsi)" "(rax <- (array-error rsi rdi))"
                   ,genStringTest "(rdx)" "(rax <- (array-error rdx 5))"
                   ,genStringTest "(rax tx)" "(rax += tx)"
                   ,genStringTest "(a rcx)" "(a <<= rcx)"
                   ,genStringTest "(a b)" "(q <- a < b)"
                   ,genStringTest "(f)" "(cjump f = 0 :a :b)"
                   ,genStringTest "(r8 r9 rcx rdi rdx rsi)" "(call :f)"
                   ,genStringTest "(r8 r9 rcx rdi rdx rsi z)" "(call z)"
                   ,genStringTest "(rsi)" "(rsi *= 2)"
                   ]


{- kill tests -}
parsrdisplayKill = printSortedList . S.toList . kill . unsafeParse . pInstruction . myLexer
killStringTest result input = TestCase $ assertEqual input result (parsrdisplayKill input)

testKill = TestList [killStringTest "()" ":f"
                    ,killStringTest "(x2)" "(x2 <- rax)"
                    ,killStringTest "(rax)" "(rax <- var)"
                    ,killStringTest "(v)" "(v <- (mem rbp -4))"
                    ,killStringTest "()" "((mem rsi -4) <- x)"
                    ,killStringTest "(x2)" "(x2 *= x2)"
                    ,killStringTest "(shmuck)" "(shmuck &= shmuck)"
                    ,killStringTest "(a)" "(a <<= rcx)"
                    ,killStringTest "(b)" "(b >>= 4)"
                    ,killStringTest "(q)" "(q <- a < b)"
                    ,killStringTest "()" "(goto :fish)"
                    ,killStringTest "()" "(cjump f = 0 :a :b)"
                    ,killStringTest "(r10 r11 r8 r9 rax rcx rdi rdx rsi)" "(call :f)"
                    ,killStringTest "()" "(tail-call :g)"
                    ,killStringTest "()" "(return)"
                    ,killStringTest "(r10 r11 r8 r9 rax rcx rdi rdx rsi)" "(rax <- (print a))"
                    ,killStringTest "(r10 r11 r8 r9 rax rcx rdi rdx rsi)" "(rax <- (allocate a 9))"
                    ,killStringTest "(r10 r11 r8 r9 rax rcx rdi rdx rsi)" "(rax <- (array-error a q))"
                    ,killStringTest "(rsi)" "(rsi <- 20)"
                    ]

foiTest input result = TestCase $ assertEqual (show input) (M.fromList result) (firstOccurrenceIdx input)

testFOI = TestList [foiTest [[1,2],[1,2]] [(1,1),(2,1)]
                   ,foiTest [[3,2,4,5],[-1,2,9,0],[]] [(3,1),(2,1),(4,1),(5,1),(-1,2),(9,2),(0,2)]
                   ]

loiTest input result = TestCase $ assertEqual (show input) (M.fromList result) (lastOccurrenceIdx input)

testLOI = TestList [loiTest [[1,2],[1,2]] [(1,2),(2,2)]
                   ,loiTest [[3,2,4,5],[-1,2,9,0],[]] [(3,1),(2,2),(4,1),(5,1),(-1,2),(9,2),(0,2)]
                   ]

