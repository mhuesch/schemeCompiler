module Glue where

import L2.Grammar

data LivenessResult = LivenessResult { iInfos :: [InstructionInfo]
                                     , allVars :: [L2Var]
                                     }

data InstructionInfo = InstructionInfo { instruct :: L2Instruction
                                       , intrfr :: [L2X]
                                       }