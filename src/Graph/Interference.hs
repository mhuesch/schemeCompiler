module Graph.Interference where


import Data.List

import Glue
import L2.AbsL2
import Graph.Graph
import Graph.GraphGlue


buildInterference :: LivenessResult -> IGraph
buildInterference (LivenessResult infos vars) = foldl instrInterfere g infos
    where
        g = foldl (flip fillEmpty) regGraph $ map (Wcx . Var) vars


instrInterfere :: IGraph -> InstructionInfo -> IGraph
instrInterfere g iInfo = foldl addMove' (foldl addFixed' g fs) ms
    where
        (ms,fs) = makeEdgePairs iInfo
        addMove' g (x1,x2) = addMove x1 x2 g
        addFixed' g (x1,x2) = addFixed x1 x2 g


makeEdgePairs :: InstructionInfo -> ([(W,W)],[(W,W)])
makeEdgePairs (InstructionInfo i interferers) = case i of
    (IAssign w1 (Sx (Xw w2))) -> let movePerms = perms [w1,w2]
                              in (movePerms, iPerms \\ movePerms)
    (IShiftCX _ _ cx) -> ([],iPerms ++ (regInterference (Wcx cx) regsMinusRCX))
    _ -> ([],iPerms)
    where
        iPerms = perms interferers
        regInterference x = concatMap (\ k -> [(x,k),(k,x)])


{- Register stuff -}
regs = colors
regsMinusRCX = filter (/= Wcx RCX) regs

regGraph :: IGraph
regGraph = fromAdj [(k1,dests k1) | k1 <- regs]
    where
        dests k = [(k2,FixedEdge) | k2 <- regs, k2 /= k]


{- Helpers -}
perms :: (Eq a) => [a] -> [(a,a)]
perms xs = [(x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2]

