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


{- Coloring -}
type Coloring = [(L2X,L2Reg)]

addColor :: L2X -> L2Reg -> Coloring -> Coloring
addColor x r cs = (x,r):cs

lookupColor :: Coloring -> L2X -> Maybe L2Reg
lookupColor cs x = lookup x cs


{- Coloring monad -}
type RMI a = ReaderT IGraph (MaybeT Identity) a

runRMI :: IGraph -> RMI a -> Maybe a
runRMI g ev = runIdentity (runMaybeT (runReaderT ev g))


{- Coloring function -}
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
        prohibitedColors = catMaybes $ map (lookupColor cs) fixedNeighbors
    case colors \\ prohibitedColors of
        [] -> fail "no colors"
        x:xs -> return $ addColor (L2Xvar v) x cs


{- Register stuff -}
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

