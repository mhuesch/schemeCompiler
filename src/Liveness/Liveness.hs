module Liveness.Liveness where


import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Set as S

import L2.Grammar
import L2.Display



data LiveSlot = LiveSlot { instr :: L2Instruction
                         , inSet :: S.Set L2X
                         , outSet :: S.Set L2X
                         , succIdxs :: [Int]
                         } deriving (Show, Eq)

type LiveArray = Array Int LiveSlot

{- Liveness function -}
liveness :: [L2Instruction] -> LiveArray
liveness = convergeLiveArray . liveListToArray

livenessLists :: [L2Instruction] -> [(L2Instruction,[L2X])]
livenessLists ls = case bounds arr of
    (0,0) -> []
    _ -> (firstIn:allOuts)
    where
        arr = liveness ls
        firstIn = (\ slot -> (instr slot, S.toList $ inSet slot)) $ arr ! 0
        allOuts = map (\ slot -> (instr slot, S.toList $ outSet slot)) $ elems arr


{- Convert function to LiveArray -}
liveListToArray :: [L2Instruction] -> LiveArray
liveListToArray ls = array (0,len-1) $ zip [0..] slots
    where
        len = length ls
        slots = map (makeSlot ls) [0..len-1]

makeSlot :: [L2Instruction] -> Int -> LiveSlot
makeSlot ls index = LiveSlot (ls !! index) S.empty S.empty (findSuccessors ls index)

findSuccessors :: [L2Instruction] -> Int -> [Int]
findSuccessors ls index = case (ls !! index) of
    (L2Goto label) -> catMaybes [elemIndex (L2ILab label) ls]
    (L2Cjump _ _ _ label1 label2) -> catMaybes [elemIndex (L2ILab label1) ls
                                               ,elemIndex (L2ILab label2) ls
                                               ]
    (L2ArrayError _ _) -> []
    (L2Return) -> []
    (L2TailCall _) -> []
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
x86CallerSaveRegX = map L2Xreg [L2EAX, L2ECX, L2EDX]


gen :: L2Instruction -> S.Set L2X
gen (L2Assign _ (L2SX x)) = liveSet [x]
gen (L2ReadMem _ x _) = liveSet [x]
gen (L2ShiftSX x1 _ x2) = liveSet [x1,x2]
gen (L2ShiftNum x _ _) = liveSet [x]
gen (L2SaveCmp _ t1 _ t2) = extractXs t1 t2
gen (L2Cjump t1 _ t2 _ _) = extractXs t1 t2
gen (L2Return) = liveSet $ resultRegX ++ calleeSaveRegX
gen (L2Allocate t1 t2) = extractXs t1 t2
gen (L2ArrayError t1 t2) = extractXs t1 t2

gen (L2Update x1 _ s) = case s of
    (L2SX x2) -> liveSet [x1,x2]
    _ -> liveSet [x1]

gen (L2Arith x1 _ t) = case t of
    (L2TX x2) -> liveSet [x1,x2]
    _ -> liveSet [x1]

gen (L2Call u) = case u of
    (L2UX x) -> liveSet $ [x] ++ argRegX
    _ -> liveSet argRegX

gen (L2TailCall u) = case u of
    (L2UX x) -> liveSet $ [x] ++ argRegX ++ calleeSaveRegX
    _ -> liveSet $ argRegX ++ calleeSaveRegX

gen (L2Print (L2TX x)) = liveSet [x]

gen _ = liveSet []


extractXs :: L2T -> L2T -> S.Set L2X
extractXs t1 t2 = case (t1,t2) of
    (L2TX x1, L2TX x2) -> liveSet [x1,x2]
    (L2TX x, L2Tnum _) -> liveSet [x]
    (L2Tnum _, L2TX x) -> liveSet [x]
    _ -> liveSet []


kill :: L2Instruction -> S.Set L2X
kill (L2Assign x _) = liveSet [x]
kill (L2ReadMem x _ _) = liveSet [x]
kill (L2Update _ _ _) = liveSet []
kill (L2Arith x _ _) = liveSet [x]
kill (L2ShiftSX x _ _) = liveSet [x]
kill (L2ShiftNum x _ _) = liveSet [x]
kill (L2SaveCmp x _ _ _) = liveSet [x]
kill (L2Call _) = liveSet $ callerSaveRegX ++ resultRegX
kill (L2Print _) = liveSet x86CallerSaveRegX
kill (L2Allocate _ _) = liveSet x86CallerSaveRegX
kill (L2ArrayError _ _) = liveSet x86CallerSaveRegX
kill _ = liveSet []




inStep :: LiveSlot -> LiveSlot
inStep slot@(LiveSlot instruction iS oS _) = slot { inSet = newInSet }
    where
        genSet = gen instruction
        killSet = kill instruction
        newInSet = S.union genSet $ S.difference oS killSet

outStep :: LiveArray -> Int -> LiveSlot
outStep arr idx = slot { outSet = inSetUnion }
    where
        slot@(LiveSlot _ _ oS succs) = arr ! idx
        successorInSets = map (inSet . (arr !)) succs
        inSetUnion = foldl (S.union) oS successorInSets


slotStep :: LiveArray -> Int -> LiveSlot
slotStep arr idx = inStep $ outStep arr idx


arrayStep :: LiveArray -> LiveArray
arrayStep arr = arr // (map (\ i -> (i,(slotStep arr i))) (indices arr))

convergeLiveArray :: LiveArray -> LiveArray
convergeLiveArray arr = if next == arr
                           then arr
                           else convergeLiveArray next
    where
        next = arrayStep arr


displayLiveArray :: LiveArray -> String
displayLiveArray arr = "((in\n" ++ (concat $ intersperse "\n" inLists)
                                ++ ")\n"
                                ++ "(out\n"
                                ++ (concat $ intersperse "\n" outLists)
                                ++ "))"
    where
        inLists = map (displayXList . S.toList . inSet) $ elems arr
        outLists = map (displayXList . S.toList . outSet) $ elems arr




