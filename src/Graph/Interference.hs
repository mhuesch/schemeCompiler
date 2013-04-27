module Graph.Interference where


import Data.List

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



buildInterference :: [(L2Instruction,[L2X])] -> IGraph
buildInterference = foldl instrInterfere regGraph


instrInterfere :: IGraph -> (L2Instruction,[L2X]) -> IGraph
instrInterfere g (_,xs) = foldl addFixed' modG perms
    where
        modG = case xs of
            [x] -> fillEmpty x g
            otherwise -> g
        addFixed' g (x1,x2) = addFixed x1 x2 g
        perms = [(x1,x2) | x1 <- xs, x2 <- xs, x1 /= x2]




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