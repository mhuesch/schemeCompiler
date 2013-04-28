module Graph.Color where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.List

import L2.Grammar
import L2.Display
import Graph.Graph
import Graph.GraphGlue



type Coloring = [(L2Var,L2Reg)]

type RMI a = ReaderT IGraph (MaybeT Identity) a

runRMI :: IGraph -> RMI a -> Maybe a
runRMI g ev = runIdentity (runMaybeT (runReaderT ev g))


colorGraph :: IGraph -> Maybe Coloring
colorGraph g = runRMI g colorComp

colorComp :: RMI Coloring
colorComp = do
    g <- ask
    let vars = concat . map getVar $ keys g
    fail "test"


getVar (L2Xvar v) = [v]
getVar _ = []



{- Display -}
displayColors :: Maybe Coloring -> String
displayColors Nothing = "#f"
displayColors (Just xs) = "(" ++ (concat . intersperse "\n" $ map displayColorPair xs) ++ ")"

displayColorPair :: (L2Var,L2Reg) -> String
displayColorPair (v,r) = "(" ++ displayVar v ++ " " ++ displayReg r ++ ")"