module Glue where

import L2.AbsL2

data LivenessResult = LivenessResult { iInfos :: [InstructionInfo]
                                     , allVars :: [Variable]
                                     }

data InstructionInfo = InstructionInfo { instruct :: Instruction
                                       , intrfr :: [W]
                                       }
