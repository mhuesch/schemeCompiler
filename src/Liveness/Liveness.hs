module Liveness.Liveness where


import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Set as S

import L2.Grammar



data LiveSlot = LiveSlot { instr :: L2Instruction
                         , inSet :: S.Set L2X
                         , outSet :: S.Set L2X
                         , succIdxs :: [Int]
                         } deriving (Show)

type LiveArray = Array Int LiveSlot


liveListToArray :: [L2Instruction] -> LiveArray
liveListToArray ls = array (0,len-1) $ zip [0..] slots
    where
        len = length ls
        slots = map (makeSlot ls) [0..len-1]

makeSlot :: [L2Instruction] -> Int -> LiveSlot
makeSlot ls index = LiveSlot (ls !! index) S.empty S.empty (successors ls index)

successors :: [L2Instruction] -> Int -> [Int]
successors ls index = case (ls !! index) of
    (L2Goto label) -> catMaybes [elemIndex (L2ILab label) ls]
    (L2Cjump _ _ _ label1 label2) -> catMaybes [elemIndex (L2ILab label1) ls
                                               ,elemIndex (L2ILab label2) ls
                                               ]
    otherwise -> if (index + 1) >= length ls
                    then []
                    else [index + 1]