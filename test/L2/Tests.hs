module L2.Tests where

import L2.Grammar
import L2.Parser
import L2.Display

import Test.HUnit
import Test.QuickCheck


main = qc

{- Quickcheck identity tests -}
prop_instruction_identity :: L2Instruction -> Bool
prop_instruction_identity x = x == (readInstruction . displayInstruction $ x)

qc = quickCheckWith progArgs prop_instruction_identity


progArgs = Args { replay = Nothing
                , maxSuccess = 10000
                , maxSize = 10000
                , maxDiscardRatio = 5
                , chatty = True
                }