module L1.Tests where

import L1.Grammar
import L1.Parser
import L1.Display

import Test.QuickCheck


main = qc

{- Quickcheck identity tests -}
prop_program_identity :: Program -> Bool
prop_program_identity x = x == (readProg . displayProgram $ x)

qc = quickCheckWith progArgs prop_program_identity


progArgs = Args { replay = Nothing
                , maxSuccess = 100
                , maxSize = 100
                , maxDiscardRatio = 5
                , chatty = True
                }