module Graph.Graph where


import qualified Data.Map as M

{- Graph type -}
type Graph k a = M.Map k (M.Map k a)

emptyGraph :: Graph k a
emptyGraph = M.empty

{- Symmetrically adds an edge -}
symInsertEdge :: Ord k => k -> k -> a -> Graph k a -> Graph k a
symInsertEdge k1 k2 v = insertEdge k1 k2 v . insertEdge k2 k1 v

{- Symmetrically apply function to possibly insert edge in graph -}
symInsertWithEdge :: Ord k => (a -> a -> a) -> k -> k -> a -> Graph k a -> Graph k a
symInsertWithEdge f k1 k2 v = insertWithEdge f k1 k2 v . insertWithEdge f k2 k1 v

{- Asymmetrically insert edge -}
insertEdge :: Ord k => k -> k -> a -> Graph k a -> Graph k a
insertEdge k1 k2 v g = case M.lookup k1 g of
    Nothing -> M.insert k1 (M.singleton k2 v) g
    (Just _) -> M.adjust (M.insert k2 v) k1 g

insertWithEdge :: Ord k => (a -> a -> a) -> k -> k -> a -> Graph k a -> Graph k a
insertWithEdge f k1 k2 v g = case M.lookup k1 g of
    Nothing -> M.insert k1 (M.singleton k2 v) g
    (Just _) -> M.adjust (M.insertWith f k2 v) k1 g


fillEmpty :: Ord k => k -> Graph k a -> Graph k a
fillEmpty k1 = M.alter f k1
    where
        f (Just subMap) = (Just subMap)
        f Nothing = (Just M.empty)


keys :: Graph k a -> [k]
keys = M.keys

fromAdj :: Ord k => [(k,[(k,a)])] -> Graph k a
fromAdj = M.fromList . map makeMap
    where
        makeMap (k,adjs) = (k, M.fromList adjs)

toAdj :: Ord k => Graph k a -> [(k,[(k,a)])]
toAdj = map innerAdj . M.toList
    where
        innerAdj (k,m) = (k,M.toList m)