module Graph.GraphGlue where


import Data.List

import L2.AbsL
import L2.PrintL
import Graph.Graph


data EdgeType = MoveEdge
              | FixedEdge
              deriving (Show, Eq)

type IGraph = Graph W EdgeType

colors :: [W]
colors = [RAX, RBP, RBX, Wcx RCX, RDI, RDX, RSI, R8, R9, R10, R11, R12, R13, R14, R15]


addFixed :: W -> W -> IGraph -> IGraph
addFixed x1 x2 = insertEdge x1 x2 FixedEdge

addMove :: W -> W -> IGraph -> IGraph
addMove x1 x2 = insertWithEdge f x1 x2 MoveEdge
    where
        f _ FixedEdge = FixedEdge
        f _ MoveEdge = MoveEdge


{- List representations -}
fixedAssocs :: IGraph -> [(W,[W])]
fixedAssocs = edgeTypeAssocs FixedEdge

moveAssocs :: IGraph -> [(W,[W])]
moveAssocs = edgeTypeAssocs MoveEdge

edgeTypeAssocs :: EdgeType -> IGraph -> [(W,[W])]
edgeTypeAssocs et = map filterFixed . toAdj
    where
        filterFixed (k,adjs) = (k, map fst $ filter ((== et) . snd) adjs)

{- Display -}
displayIGraph :: IGraph -> String
displayIGraph g = "(" ++ (intercalate "\n" . sort $ rows) ++ ")"
    where
        rows = map displayIAssoc . fixedAssocs $ g

displayIAssoc :: (W,[W]) -> String
displayIAssoc (k1, ks) = "(" ++ unwords (displayedK1:displayedKs) ++ ")"
    where
        displayedK1 = printTree k1
        displayedKs = sort $ map printTree ks
