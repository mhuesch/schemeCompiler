module L5ToBinary.Compile where


--

import L5.Grammar
import qualified L1Tox86.Compile as L1c
import qualified L2ToL1.Compile as L2c
import qualified L3ToL2.Compile as L3c
import qualified L4ToL3.Compile as L4c
import qualified L5ToL4.Compile as L5c


translate :: L5Program -> Either String String
translate = fmap L1c.generateAssembly . L2c.translate . L3c.translate . L4c.translate . L5c.translate

