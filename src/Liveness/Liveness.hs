module Liveness.Liveness where


import Data.List
import Data.Maybe
import Data.Array
import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M

import Glue
import L2.AbsL2
import L2.PrintL2



data LiveSlot = LiveSlot { instr :: Instruction
                         , inSet :: S.Set W
                         , outSet :: S.Set W
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
        filterKills slot = kill $ instr slot
        firstIn = (\ slot -> InstructionInfo (instr slot) (S.toList $ inSet slot)) $ arr ! 0
        allOuts = map (\ slot -> InstructionInfo (instr slot) (S.toList $ S.union (filterKills slot) (outSet slot))) $ elems arr

costSortedVars :: [InstructionInfo] -> [Variable]
costSortedVars infos = map fst . sortBy (compare `on` snd) $ weightedAssocs
    where
        weightedAssocs = computeWeighting interferingVars
        interferingVars = map (concatMap getVars . intrfr) infos
        getVars (Wcx (Var v)) = [v]
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
findSuccessors ls idx = case ls !! idx of
    (IGoto label) -> catMaybes [elemIndex (ILabel label) ls]
    (ICjump _ _ _ label1 label2) -> catMaybes [elemIndex (ILabel label1) ls
                                               ,elemIndex (ILabel label2) ls
                                               ]
    (ICallRuntime ArrayError _) -> []
    IReturn                     -> []
    ITailCall{}                 -> []
    _                           -> if (idx + 1) >= length ls
                                      then []
                                      else [idx + 1]


liveSet :: [W] -> S.Set W
liveSet = S.fromList

{- Specific registers -}
argRegW :: [W]
argRegW = [RDI, RSI, RDX, (Wcx RCX), R8, R9]

resultRegW :: [W]
resultRegW = [RAX]

callerSaveRegW :: [W]
callerSaveRegW = [R10, R11]

calleeSaveRegW :: [W]
calleeSaveRegW = [RBX, RBP, R12, R13, R14, R15]

gen :: Instruction -> S.Set W
gen (IAssign _ (Sx (Xw w))) = liveSet [w]
gen (IReadMem _ (Xw w) _) = liveSet [w]
gen (IShiftCX w _ cx) = liveSet [w,(Wcx cx)]
gen (IShiftN w _ _) = liveSet [w]
gen (ISaveCmp _ t1 _ t2) = extractWs t1 t2
gen (ICjump t1 _ t2 _ _) = extractWs t1 t2
gen (IReturn) = liveSet $ resultRegW ++ calleeSaveRegW
--
gen (IWriteMem x _ s) = case (x,s) of
    (Xw w1, Sx (Xw w2)) -> liveSet [w1,w2]
    (_, Sx (Xw w)) -> liveSet [w]
    (Xw w, _) -> liveSet [w]
    _ -> liveSet []
--
gen (IArith w1 _ t) = case t of
    (Tx (Xw w2)) -> liveSet [w1,w2]
    _ -> liveSet [w1]
--
gen (ICallNative u arity) = case u of
    (Ux (Xw w)) -> liveSet $ w : argsByArity arity
    _ -> liveSet $ argsByArity arity
--
gen (ICallRuntime _ arity) = liveSet $ argsByArity arity
--
gen (ITailCall u arity) = case u of
    (Ux (Xw w)) -> liveSet $ w : calleeSaveRegW ++ argsByArity arity
    _ -> liveSet $ calleeSaveRegW ++ argsByArity arity
--
gen _ = liveSet []


argsByArity :: PosNegInteger -> [W]
argsByArity arity = take (pniToInt arity) argRegW

extractWs :: T -> T -> S.Set W
extractWs t1 t2 = case (t1,t2) of
    (Tx (Xw w1), Tx (Xw w2)) -> liveSet [w1,w2]
    (Tx (Xw w), Tnum _) -> liveSet [w]
    (Tnum _, Tx (Xw w)) -> liveSet [w]
    _ -> liveSet []


kill :: Instruction -> S.Set W
kill (IAssign w _) = liveSet [w]
kill (IReadMem w _ _) = liveSet [w]
kill (IWriteMem{}) = liveSet []
kill (IArith w _ _) = liveSet [w]
kill (IShiftCX w _ _) = liveSet [w]
kill (IShiftN w _ _) = liveSet [w]
kill (ISaveCmp w _ _ _) = liveSet [w]
kill (ICallNative _ _) = liveSet $ resultRegW ++ callerSaveRegW ++ argRegW
kill (ICallRuntime _ _) = liveSet $ resultRegW ++ callerSaveRegW ++ argRegW
kill _ = liveSet []


pniToInt :: PosNegInteger -> Int
pniToInt (PosNegInteger s) = read s


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
arrayStep arr = arr // map (\ i -> (i, slotStep arr i)) (indices arr)

convergeLiveArray :: LiveArray -> LiveArray
convergeLiveArray arr = if next == arr
                           then arr
                           else convergeLiveArray next
    where
        next = arrayStep arr


displayLiveArray :: LiveArray -> String
displayLiveArray arr = "((in\n" ++ unlines inLists
                                ++ ")\n"
                                ++ "(out\n"
                                ++ unlines outLists
                                ++ "))"
    where
        inLists = map (printSortedList . S.toList . inSet) $ elems arr
        outLists = map (printSortedList . S.toList . outSet) $ elems arr



printSortedList :: (Print a, Ord a) => [a] -> String
printSortedList as = "(" ++ (unwords . sort . map printTree $ as) ++ ")"

