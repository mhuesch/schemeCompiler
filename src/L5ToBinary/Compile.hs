module L5ToBinary.Compile where

import qualified L5.AbsL5 as L5
import qualified L1Tox64.Compile as L1c
import qualified L2ToL1.Compile as L2c
import qualified L3ToL2.Compile as L3c
import qualified L4ToL3.Compile as L4c
import qualified L5ToL4.Compile as L5c


translate :: L5.Program -> Either String String
translate = fmap L1c.assembleProgram . L2c.translate . L3c.translate . L4c.translate . L5c.translate

