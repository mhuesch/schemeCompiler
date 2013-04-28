module Graph.GraphGlue where


import L2.Grammar
import Graph.Graph


data EdgeType = MoveEdge
              | FixedEdge
              deriving (Show, Eq)

type IGraph = Graph L2X EdgeType