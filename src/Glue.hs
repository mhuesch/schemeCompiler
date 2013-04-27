module Glue where

import L2.Grammar

data LivenessResult = LivenessResult { allXs :: [L2X]
                                     , iInfos :: [InstructionInfo]
                                     }

data InstructionInfo = InstructionInfo { instruct :: L2Instruction
                                       , intrfr :: [L2X]
                                       }