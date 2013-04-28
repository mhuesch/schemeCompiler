module Graph.GraphGlue where


import L2.Grammar
import Graph.Graph


data EdgeType = MoveEdge
              | FixedEdge
              deriving (Show, Eq)

type IGraph = Graph L2X EdgeType




{- List representations -}
fixedAssocs :: IGraph -> [(L2X,[L2X])]
fixedAssocs = edgeTypeAssocs FixedEdge

moveAssocs :: IGraph -> [(L2X,[L2X])]
moveAssocs = edgeTypeAssocs MoveEdge

edgeTypeAssocs :: EdgeType -> IGraph -> [(L2X,[L2X])]
edgeTypeAssocs et = map filterFixed . toAdj
    where
        filterFixed (k,adjs) = (k, map fst $ filter ((== et) . snd) adjs)





addFixed :: L2X -> L2X -> IGraph -> IGraph
addFixed x1 x2 = insertEdge x1 x2 FixedEdge

addMove :: L2X -> L2X -> IGraph -> IGraph
addMove x1 x2 = insertWithEdge f x1 x2 MoveEdge
    where
        f _ FixedEdge = FixedEdge
        f _ MoveEdge = MoveEdge



removeSym :: Eq a => [(a,a)] -> [(a,a)]
removeSym [] = []
removeSym ((x,y):vs) = (x,y):(removeSym $ filter (\ p -> p /= (y,x)) vs)