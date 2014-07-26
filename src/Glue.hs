module Glue where

import L2.AbsL2

data LivenessResult = LivenessResult { iInfos :: [InstructionInfo]
                                     , allVars :: [Variable]
                                     }

data InstructionInfo = InstructionInfo { instruct :: Instruction
                                       , intrfr :: [W]
                                       }

{- Specific registers -}
argRegW :: [W]
argRegW = [RDI, RSI, RDX, (Wcx RCX), R8, R9]

resultRegW :: [W]
resultRegW = [RAX]

callerSaveRegW :: [W]
callerSaveRegW = [R10, R11]

calleeSaveRegW :: [W]
calleeSaveRegW = [RBX, RBP, R12, R13, R14, R15]
