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


{- Convert function to LiveArray -}
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


isLive :: L2X -> Bool
isLive (L2Xreg L2EBP) = False
isLive (L2Xreg L2ESP) = False
isLive _ = True

liveSet :: [L2X] -> S.Set L2X
liveSet = S.fromList . filter isLive

{- Specific registers -}
argRegX :: [L2X]
argRegX = map L2Xreg [L2EAX, L2ECX, L2EDX]

resultRegX :: [L2X]
resultRegX = map L2Xreg [L2EAX]

callerSaveRegX :: [L2X]
callerSaveRegX = map L2Xreg [L2EAX, L2EBX, L2ECX, L2EDX]

calleeSaveRegX :: [L2X]
calleeSaveRegX = map L2Xreg [L2EDI, L2ESI]

x86CallerSaveRegX :: [L2X]
x86CallerSaveRegX = map L2Xreg [L2EAX, L2EBX, L2ECX]

gen :: L2Instruction -> S.Set L2X
gen (L2Assign _ (L2SX x)) = liveSet [x]
gen (L2ReadMem _ x _) = liveSet [x]
gen (L2Update _ _ (L2SX x)) = liveSet [x]
gen (L2Arith x1 _ (L2TX x2)) = liveSet [x1,x2]
gen (L2ShiftSX x1 _ x2) = liveSet [x1,x2]
gen (L2ShiftNum x _ _) = liveSet [x]
gen (L2SaveCmp _ (L2TX x1) _ (L2TX x2)) = liveSet [x1,x2]
gen (L2Cjump (L2TX x1) _ (L2TX x2) _ _) = liveSet [x1,x2]
gen (L2Call (L2UX x)) = liveSet $ [x] ++ argRegX
gen (L2TailCall (L2UX x)) = liveSet $ [x] ++ argRegX ++ calleeSaveRegX
gen (L2Return) = liveSet $ resultRegX ++ calleeSaveRegX
gen (L2Print (L2TX x)) = liveSet [x]
gen (L2Allocate t1 t2) = case (t1,t2) of
    (L2TX x1, L2TX x2) -> liveSet [x1,x2]
    (L2TX x, L2Tnum _) -> liveSet [x]
    (L2Tnum _, L2TX x) -> liveSet [x]
    otherwise -> liveSet []
gen (L2ArrayError t1 t2) = case (t1,t2) of
    (L2TX x1, L2TX x2) -> liveSet [x1,x2]
    (L2TX x, L2Tnum _) -> liveSet [x]
    (L2Tnum _, L2TX x) -> liveSet [x]
    otherwise -> liveSet []
gen _ = liveSet []







