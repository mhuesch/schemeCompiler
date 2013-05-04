module Graph.Interference where


import Data.List

import Glue
import L2.Grammar
import Graph.Graph
import Graph.GraphGlue


buildInterference :: LivenessResult -> IGraph
buildInterference (LivenessResult infos vars) = foldl instrInterfere g infos
    where
        g = foldl (flip fillEmpty) regGraph $ map L2Xvar vars


instrInterfere :: IGraph -> InstructionInfo -> IGraph
instrInterfere g iInfo = foldl addMove' (foldl addFixed' g fs) ms
    where
        (ms,fs) = makeEdgePairs iInfo
        addMove' g (x1,x2) = addMove x1 x2 g
        addFixed' g (x1,x2) = addFixed x1 x2 g


makeEdgePairs :: InstructionInfo -> ([(L2X,L2X)],[(L2X,L2X)])
makeEdgePairs (InstructionInfo i interferers) = case i of
    (L2Assign x1 (L2SX x2)) -> let movePerms = perms . onlyLive $ [x1,x2]
                               in (movePerms, iPerms \\ movePerms)
    (L2SaveCmp x _ _ _) -> ([],iPerms ++ (regInterference x [L2ESI, L2EDI]))
    (L2ShiftSX _ _ x) -> ([],iPerms ++ (regInterference x [L2EAX,L2EBX,L2EDI,L2EDX,L2ESI]))
    _ -> ([],iPerms)
    where
        iPerms = perms interferers
        regInterference x = concatMap (\ k -> [(x,k),(k,x)]) . map L2Xreg


{- Register stuff -}
regs = map L2Xreg [L2EAX, L2EBX, L2ECX, L2EDI, L2EDX, L2ESI]

regGraph :: IGraph
regGraph = fromAdj [(k1,dests k1) | k1 <- regs]
    where
        dests k = [(k2,FixedEdge) | k2 <- regs, k2 /= k]


{- Helpers -}
perms :: (Eq a) => [a] -> [(a,a)]
perms xs = [(x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2]

onlyLive :: [L2X] -> [L2X]
onlyLive = filter isLive

isLive :: L2X -> Bool
isLive (L2Xreg L2EBP) = False
isLive (L2Xreg L2ESP) = False
isLive _ = True

