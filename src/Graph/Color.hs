module Graph.Color where


import Data.Maybe
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.List

import L2.AbsL2
import L2.PrintL2
import Graph.Graph
import Graph.GraphGlue


{- Coloring -}
type Coloring = [(W,W)]

addColor :: W -> W -> Coloring -> Coloring
addColor w r cs = (w,r):cs

lookupColor :: Coloring -> W -> Maybe W
lookupColor cs w = lookup w cs


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
    let vars = keys g
    foldM attemptColor coloredRegs vars


attemptColor :: Coloring -> W -> RMI Coloring
attemptColor cs w = do
    g <- ask
    let fixedNeighbors = filterNeighbors w FixedEdge g
        prohibitedColors = mapMaybe (lookupColor cs) fixedNeighbors
    case colors \\ prohibitedColors of
        []  -> fail "no colors"
        c:_ -> return $ addColor w c cs


{- Register stuff -}
coloredRegs :: Coloring
coloredRegs = map (\ w -> (w, w)) colors


{- Display -}
displayColors :: Maybe Coloring -> String
displayColors Nothing = "#f"
displayColors (Just xs) = "(" ++ (intercalate "\n" . sort . map displayVarColor . getVarColors $ xs) ++ ")"

getVarColors :: Coloring -> [(Variable,W)]
getVarColors = concatMap f
    where
        f (Wcx (Var v), r) = [(v,r)]
        f _ = []

displayVarColor :: (Variable,W) -> String
displayVarColor (v,r) = "(" ++ printTree v ++ " " ++ printTree r ++ ")"

