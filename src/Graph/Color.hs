module Graph.Color where


import Data.Maybe
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.List

import L2.Grammar
import L2.Display
import Graph.Graph
import Graph.GraphGlue


{- Coloring monad -}
type Coloring = [(L2X,L2Reg)]

type RMI a = ReaderT IGraph (MaybeT Identity) a

runRMI :: IGraph -> RMI a -> Maybe a
runRMI g ev = runIdentity (runMaybeT (runReaderT ev g))
{---------}



colorGraph :: IGraph -> Maybe Coloring
colorGraph g = liftM refillColors $ runColor newG
    where
        refillColors colors = (fillReplacedColors colors replaced) ++ colors
        (replaced,newG) = foldl replaceMove ([],g) (asymMovePairs g)


fillReplacedColors :: Coloring -> [(L2Var,L2X)] -> Coloring
fillReplacedColors colors [] = []
fillReplacedColors colors ((v,x):ls) = case lookup x colors of
            Nothing -> error $ ", X:" ++ show x ++ ", colors:" ++ show colors
            (Just r) -> (L2Xvar v,r):fillReplacedColors colors ls

replaceMove :: ([(L2Var,L2X)],IGraph) -> (L2X,L2X) -> ([(L2Var,L2X)],IGraph)
replaceMove (replaced,g) pair = case pair of
    (x1@(L2Xvar v1), x2@(L2Xvar v2)) -> handleCombination replaced v2 x1 g
    (x1@(L2Xvar v), x2@(L2Xreg _)) -> handleCombination replaced v x2 g
    (x1@(L2Xreg _), x2@(L2Xvar v)) -> handleCombination replaced v x1 g
    (L2Xreg _,L2Xreg _) -> (replaced,g)

handleCombination :: [(L2Var,L2X)] -> L2Var -> L2X -> IGraph -> ([(L2Var,L2X)],IGraph)
handleCombination replaced v x g = case combineNodesSize x (L2Xvar v) 6 g of
    Nothing -> (replaced,g)
    (Just newG) -> ((v,x):replaced,newG)



asymMovePairs :: IGraph -> [(L2X,L2X)] 
asymMovePairs = removeSym . movePairs

movePairs :: IGraph -> [(L2X,L2X)] 
movePairs = concatMap f . moveAssocs
    where
        f (x1,xs) = map (\ x2 -> (x1,x2)) xs




{- Color by fixed edges -}
runColor :: IGraph -> Maybe Coloring
runColor g = runRMI g colorComp


colorComp :: RMI Coloring
colorComp = do
    g <- ask
    let f (L2Xvar v) = [v]
        f _ = []
        vars = concatMap f $ keys g
    foldM attemptColor coloredRegs vars


attemptColor :: Coloring -> L2Var -> RMI Coloring
attemptColor cs v = do
    g <- ask
    let fixedNeighbors = filterNeighbors (L2Xvar v) FixedEdge g
        prohibitedColors = catMaybes $ map (flip lookup $ cs) fixedNeighbors
    case colors \\ prohibitedColors of
        [] -> fail "no colors"
        xs -> return $ ((L2Xvar v),head xs):cs


{- Constants -}
coloredRegs :: Coloring
coloredRegs = map (\ r -> (L2Xreg r, r)) colors

colors :: [L2Reg]
colors = [L2EBX, L2ECX, L2EDX, L2EDI, L2ESI, L2EAX]



{- Display -}
displayColors :: Maybe Coloring -> String
displayColors Nothing = "#f"
displayColors (Just xs) = "(" ++ (concat . intersperse "\n" . sort . map displayVarColor . getVarColors $ xs) ++ ")"

getVarColors :: Coloring -> [(L2Var,L2Reg)]
getVarColors = concat . map f
    where
        f ((L2Xvar v),r) = [(v,r)]
        f _ = []

displayVarColor :: (L2Var,L2Reg) -> String
displayVarColor (v,r) = "(" ++ displayVar v ++ " " ++ displayReg r ++ ")"

