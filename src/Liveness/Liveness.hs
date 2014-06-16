module Liveness.Liveness where


import Data.List
import Data.Maybe
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

import Glue
import L2.AbsL2
import L2.PrintL2



data LiveSlot = LiveSlot { instr :: Instruction
                         , inSet :: S.Set X
                         , outSet :: S.Set X
                         , succIdxs :: [Int]
                         } deriving (Show, Eq)

type LiveArray = Array Int LiveSlot

{- Liveness function -}
liveness :: [Instruction] -> LiveArray
liveness = convergeLiveArray . liveListToArray

liveRes :: [Instruction] -> LivenessResult
liveRes ls = LivenessResult infos vars
    where
        arr = liveness ls
        vars = costSortedVars infos
        infos = case bounds arr of
            (0,-1) -> []
            _ -> firstIn:allOuts
        filterKills slot = S.filter isLive . kill $ instr slot
        firstIn = (\ slot -> InstructionInfo (instr slot) (S.toList $ inSet slot)) $ arr ! 0
        allOuts = map (\ slot -> InstructionInfo (instr slot) (S.toList $ S.union (filterKills slot) (outSet slot))) $ elems arr

costSortedVars :: [InstructionInfo] -> [Variable]
costSortedVars infos = map fst . sortBy (\ x y -> compare (snd x) (snd y)) $ weightedAssocs
    where
        weightedAssocs = computeWeighting interferingVars
        interferingVars = map (concatMap getVars . intrfr) infos
        getVars (Xw (Wcx (Var v))) = [v]
        getVars _ = []

computeWeighting :: (Ord a) => [[a]] ->[(a,Int)]
computeWeighting ls = map weightTuple $ zip4 (M.keys varCountMap)
                                             (M.elems varCountMap)
                                             (M.elems firstIdxMap)
                                             (M.elems lastIdxMap)
    where
        varCountMap = countVarOccurrences $ concat ls
        firstIdxMap = firstOccurrenceIdx ls
        lastIdxMap = lastOccurrenceIdx ls

weightTuple :: (a,Int,Int,Int) -> (a,Int)
weightTuple (x,count,firstIdx,lastIdx) = (x,weight)
    where
        weight = (rangeConstant * liveRange) + (countConstant * count)
        rangeConstant = -9
        countConstant = 8
        liveRange = lastIdx - firstIdx

countVarOccurrences :: (Ord a) => [a] -> M.Map a Int
countVarOccurrences = foldl (flip $ M.alter incOrAdd) M.empty
    where
        incOrAdd (Nothing) = Just 1
        incOrAdd (Just n) = Just (n+1)

firstOccurrenceIdx :: (Ord a) => [[a]] -> M.Map a Int
firstOccurrenceIdx = customOccurencesIndex [1..]

lastOccurrenceIdx :: (Ord a) => [[a]] -> M.Map a Int
lastOccurrenceIdx ls = customOccurencesIndex [len,len-1..] (reverse ls)
    where
        len = length ls

customOccurencesIndex :: (Ord a) => [Int] -> [[a]] -> M.Map a Int
customOccurencesIndex idxs = foldl f M.empty . zip idxs
    where
        f m (i,xs) = foldl (\ acc x -> case M.lookup x acc of
                                            Nothing -> M.insert x i acc
                                            (Just _) -> acc)
                           m
                           xs




{- Convert function to LiveArray -}
liveListToArray :: [Instruction] -> LiveArray
liveListToArray ls = array (0,len-1) $ zip [0..] slots
    where
        len = length ls
        slots = map (makeSlot ls) [0..len-1]

makeSlot :: [Instruction] -> Int -> LiveSlot
makeSlot ls idx = LiveSlot (ls !! idx) S.empty S.empty (findSuccessors ls idx)

findSuccessors :: [Instruction] -> Int -> [Int]
findSuccessors ls idx = case (ls !! idx) of
    (IGoto label) -> catMaybes [elemIndex (ILabel label) ls]
    (ICjump _ _ _ label1 label2) -> catMaybes [elemIndex (ILabel label1) ls
                                               ,elemIndex (ILabel label2) ls
                                               ]
    (IArrayError{}) -> []
    (IReturn)       -> []
    (ITailCall _)   -> []
    _               -> if (idx + 1) >= length ls
                          then []
                          else [idx + 1]


isLive :: X -> Bool
isLive RSP = False
isLive _ = True

liveSet :: [X] -> S.Set X
liveSet = S.fromList . filter isLive

{- Specific registers -}
argRegX :: [X]
argRegX = map Xw [RDI, RSI, RDX, (Wcx RCX), R8, R9]

resultRegX :: [X]
resultRegX = [Xw RAX]

callerSaveRegX :: [X]
callerSaveRegX = map Xw [R10, R11]

calleeSaveRegX :: [X]
calleeSaveRegX = map Xw [RBX, RBP, R12, R13, R14, R15]

gen :: Instruction -> S.Set X
gen (IAssign _ (Sx x)) = liveSet [x]
gen (IReadMem _ x _) = liveSet [x]
gen (IShiftCX w _ cx) = liveSet [(Xw w),(Xw (Wcx cx))]
gen (IShiftN w _ _) = liveSet [Xw w]
gen (ISaveCmp _ t1 _ t2) = extractXs t1 t2
gen (ICjump t1 _ t2 _ _) = extractXs t1 t2
gen (IReturn) = liveSet $ resultRegX ++ calleeSaveRegX
gen (IPrint _ (Tx x)) = liveSet [x]
gen (IAllocate _ t1 t2) = extractXs t1 t2
gen (IArrayError _ t1 t2) = extractXs t1 t2
--
gen (IWriteMem x1 _ s) = case s of
    (Sx x2) -> liveSet [x1,x2]
    _ -> liveSet [x1]
--
gen (IArith w _ t) = case t of
    (Tx x) -> liveSet [Xw w,x]
    _ -> liveSet [Xw w]
--
gen (ICall u) = case u of
    (Ux x) -> liveSet $ x : argRegX
    _ -> liveSet argRegX
--
gen (ITailCall u) = case u of
    (Ux x) -> liveSet $ x : argRegX ++ calleeSaveRegX
    _ -> liveSet $ argRegX ++ calleeSaveRegX
--
gen _ = liveSet []


extractXs :: T -> T -> S.Set X
extractXs t1 t2 = case (t1,t2) of
    (Tx x1, Tx x2) -> liveSet [x1,x2]
    (Tx x, Tnum _) -> liveSet [x]
    (Tnum _, Tx x) -> liveSet [x]
    _ -> liveSet []


kill :: Instruction -> S.Set X
kill (IAssign w _) = liveSet [Xw w]
kill (IReadMem w _ _) = liveSet [Xw w]
kill (IWriteMem{}) = liveSet []
kill (IArith w _ _) = liveSet [Xw w]
kill (IShiftCX w _ _) = liveSet [Xw w]
kill (IShiftN w _ _) = liveSet [Xw w]
kill (ISaveCmp w _ _ _) = liveSet [Xw w]
kill (ICall _) = liveSet $ resultRegX ++ callerSaveRegX ++ argRegX
kill (IPrint{}) = liveSet $ resultRegX ++ callerSaveRegX ++ argRegX
kill (IAllocate{}) = liveSet $ resultRegX ++ callerSaveRegX ++ argRegX
kill (IArrayError{}) = liveSet $ resultRegX ++ callerSaveRegX ++ argRegX
kill _ = liveSet []




inStep :: LiveSlot -> LiveSlot
inStep slot@(LiveSlot instruction _ oS _) = slot { inSet = newInSet }
    where
        genSet = gen instruction
        killSet = kill instruction
        newInSet = S.union genSet $ S.difference oS killSet

outStep :: LiveArray -> Int -> LiveSlot
outStep arr idx = slot { outSet = inSetUnion }
    where
        slot@(LiveSlot _ _ oS succs) = arr ! idx
        successorInSets = map (inSet . (arr !)) succs
        inSetUnion = foldl S.union oS successorInSets


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
        inLists = map (printSortedList . S.toList . inSet) $ elems arr
        outLists = map (printSortedList . S.toList . outSet) $ elems arr



printSortedList :: [X] -> String
printSortedList xs = "(" ++ (unwords . sort . map printTree $ xs) ++ ")"

