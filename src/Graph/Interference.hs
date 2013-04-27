module Graph.Interference where


import Data.List

import Glue
import L2.Grammar
import L2.Display
import Graph.Graph


data EdgeType = MoveEdge
              | FixedEdge
              deriving (Show, Eq)

type IGraph = Graph L2X EdgeType


regs = map L2Xreg [L2EAX, L2EBX, L2ECX, L2EDI, L2EDX, L2ESI]

regGraph :: IGraph
regGraph = fromAdj [(k1,dests k1) | k1 <- regs]
    where
        dests k = [(k2,FixedEdge) | k2 <- regs, k2 /= k]



buildInterference :: LivenessResult -> IGraph
buildInterference (LivenessResult xs infos) = foldl instrInterfere g infos
    where
        g = foldl (flip fillEmpty) regGraph xs


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
        regInterference x = concat . map (\ k -> [(x,k),(k,x)]) . map L2Xreg


perms xs = [(x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2]


{-
instructionInterference :: L2Instruction -> ([(L2X,L2X)],[(L2X,L2X)])
instructionInterference (L2Assign x1 (L2SX x2)) = (onlyLive [x1,x2],[])
instructionInterference L2SaveCmp{} = ([],map L2Xreg [L2ESI, L2EDI])
instructionInterference L2ShiftSX{} = ([],map L2Xreg [L2EAX,L2EBX,L2EDI,L2EDX,L2ESI])
instructionInterference _ = ([],[])
-}




onlyLive :: [L2X] -> [L2X]
onlyLive = filter isLive

isLive :: L2X -> Bool
isLive (L2Xreg L2EBP) = False
isLive (L2Xreg L2ESP) = False
isLive _ = True








addFixed :: L2X -> L2X -> IGraph -> IGraph
addFixed x1 x2 = insertEdge x1 x2 FixedEdge

addMove :: L2X -> L2X -> IGraph -> IGraph
addMove x1 x2 = insertWithEdge f x1 x2 MoveEdge
    where
        f _ FixedEdge = FixedEdge
        f _ MoveEdge = MoveEdge


{- List representations -}
fixedAssocs :: IGraph -> [(L2X,[L2X])]
fixedAssocs = edgeTypeAssocs FixedEdge

moveAssocs :: IGraph -> [(L2X,[L2X])]
moveAssocs = edgeTypeAssocs MoveEdge

edgeTypeAssocs :: EdgeType -> IGraph -> [(L2X,[L2X])]
edgeTypeAssocs et = map filterFixed . toAdj
    where
        filterFixed (k,adjs) = (k, map fst $ filter ((== et) . snd) adjs)

{- Display -}
displayIGraph :: IGraph -> String
displayIGraph g = "(" ++ (concat . intersperse "\n" . sort $ rows) ++ ")"
    where
        rows = map displayIAssoc . fixedAssocs $ g

displayIAssoc :: (L2X,[L2X]) -> String
displayIAssoc (k1, ks) = "(" ++ (concat . intersperse " " $ displayedK1:displayedKs) ++ ")"
    where
        displayedK1 = displayX k1
        displayedKs = sort $ map displayX ks