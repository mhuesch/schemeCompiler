module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import System.Exit (exitFailure)
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

import L2.Grammar
import L2.Parser
import L2.Display


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- parseFromFile parseSpill (args !! 0)
    case result of
        Left err -> putStrLn . show $ err
        Right sp -> putStrLn . displayInstrList . spill $ sp


spill :: L2Spill -> [L2Instruction]
spill (L2Spill ls var offset prefix) = res
    where (res, _) = runReadState (var, offset, prefix) 0 (spillInstructions ls)


type RS a = ReaderT Env (StateT Int Identity) a
type Env = (L2Var, Int, L2Var)

runReadState :: Env -> Int -> RS a -> (a, Int)
runReadState env st rs = runIdentity (runStateT (runReaderT rs env) st)


spillInstructions :: [L2Instruction] -> RS [L2Instruction]
spillInstructions ls = liftM concat $ mapM spillInstruction ls


spillInstruction :: L2Instruction -> RS [L2Instruction]
spillInstruction i@(L2Assign x s) = do
    (var,varOffset,prefix) <- ask
    case ((x == L2Xvar var),(s == L2SX (L2Xvar var))) of
        (True,True) -> return []
        (True,False) -> return [mkUpdate varOffset s]
        (False,True) -> return [mkRead x varOffset]
        (False,False) -> return [i]

spillInstruction i@(L2ReadMem x1 x2 readOffset) = do
    (var,varOffset,prefix) <- ask
    case ((x1 == L2Xvar var),(x2 == L2Xvar var)) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ReadMem (L2Xvar newVar) (L2Xvar newVar) readOffset)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(L2ReadMem (L2Xvar newVar) x2 readOffset)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ReadMem x1 (L2Xvar newVar) readOffset)]
        (False,False) -> return [i]

spillInstruction i@(L2Update x updateOffset s) = do
    (var,varOffset,prefix) <- ask
    case ((x == L2Xvar var),(s == L2SX (L2Xvar var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Update (L2Xvar newVar) updateOffset (L2SX (L2Xvar newVar)))
                   ]
        (True, False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Update (L2Xvar newVar) updateOffset s)
                   ]
        (False, True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Update x updateOffset (L2SX (L2Xvar newVar)))
                   ]
        (False, False) -> return [i]

spillInstruction i@(L2Arith x aop t) = do
    (var,varOffset,prefix) <- ask
    case ((x == L2Xvar var),(t == L2TX (L2Xvar var))) of
        (True, True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Arith (L2Xvar newVar) aop (L2TX (L2Xvar newVar)))
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True, False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Arith (L2Xvar newVar) aop t)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (False, True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Arith x aop (L2TX (L2Xvar newVar)))
                   ]
        (False, False) -> return [i]

spillInstruction i@(L2ShiftSX x1 sop x2) = do
    (var,varOffset,prefix) <- ask
    case ((x1 == L2Xvar var),(x2 == (L2Xvar var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ShiftSX (L2Xvar newVar) sop (L2Xvar newVar))
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ShiftSX (L2Xvar newVar) sop x2)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ShiftSX x1 sop (L2Xvar newVar))
                   ]
        (False,False) -> return [i]

spillInstruction i@(L2ShiftNum x sop n) = do
    (var,varOffset,prefix) <- ask
    case (x == L2Xvar var) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ShiftNum (L2Xvar newVar) sop n)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        False -> return [i]

spillInstruction i@(L2SaveCmp x t1 cmp t2) = do
    (var,varOffset,prefix) <- ask
    case ((x == L2Xvar var),(t1 == (L2TX (L2Xvar var))),(t2 == (L2TX (L2Xvar var)))) of
        (True,True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp (L2Xvar newVar) (L2TX (L2Xvar newVar)) cmp (L2TX (L2Xvar newVar)))
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True,True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp (L2Xvar newVar) (L2TX (L2Xvar newVar)) cmp t2)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True,False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp (L2Xvar newVar) t1 cmp (L2TX (L2Xvar newVar)))
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (True,False,False) -> do
            newVar <- genVar prefix
            return [(L2SaveCmp (L2Xvar newVar) t1 cmp t2)
                   ,(mkUpdate varOffset (L2SX (L2Xvar newVar)))
                   ]
        (False,True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp x (L2TX (L2Xvar newVar)) cmp (L2TX (L2Xvar newVar)))
                   ] 
        (False,True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp x (L2TX (L2Xvar newVar)) cmp t2)
                   ] 
        (False,False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2SaveCmp x t1 cmp (L2TX (L2Xvar newVar)))
                   ]
        (False,False,False) -> return [i]

spillInstruction i@(L2ILab _) = return [i]
spillInstruction i@(L2Goto _) = return [i]

spillInstruction i@(L2Cjump t1 cmp t2 l1 l2) = do
    (var,varOffset,prefix) <- ask
    case ((t1 == L2TX (L2Xvar var)),(t2 == L2TX (L2Xvar var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Cjump (L2TX (L2Xvar newVar)) cmp (L2TX (L2Xvar newVar)) l1 l2)
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Cjump (L2TX (L2Xvar newVar)) cmp t2 l1 l2)
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Cjump t1 cmp (L2TX (L2Xvar newVar)) l1 l2)
                   ]
        (False,False) -> return [i]

spillInstruction i@(L2Call u) = do
    (var,varOffset,prefix) <- ask
    case (u == L2UX (L2Xvar var)) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Call (L2UX (L2Xvar newVar)))
                   ]
        False -> return [i]

spillInstruction i@(L2TailCall u) = do
    (var,varOffset,prefix) <- ask
    case (u == L2UX (L2Xvar var)) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2TailCall (L2UX (L2Xvar newVar)))
                   ]
        False -> return [i]

spillInstruction i@(L2Return) = return [i]

spillInstruction i@(L2Print x t) = do
    (var,varOffset,prefix) <- ask
    case (t == L2TX (L2Xvar var)) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Print x (L2TX (L2Xvar newVar)))
                   ]
        False -> return [i]
        
spillInstruction i@(L2Allocate x t1 t2) = do
    (var,varOffset,prefix) <- ask
    case ((t1 == L2TX (L2Xvar var)),(t2 == L2TX (L2Xvar var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Allocate x (L2TX (L2Xvar newVar)) (L2TX (L2Xvar newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Allocate x (L2TX (L2Xvar newVar)) t2)
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2Allocate x t1 (L2TX (L2Xvar newVar)))
                   ]
        (False,False) -> return [i]

spillInstruction i@(L2ArrayError x t1 t2) = do
    (var,varOffset,prefix) <- ask
    case ((t1 == L2TX (L2Xvar var)),(t2 == L2TX (L2Xvar var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ArrayError x (L2TX (L2Xvar newVar)) (L2TX (L2Xvar newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ArrayError x (L2TX (L2Xvar newVar)) t2)
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (L2Xvar newVar) varOffset)
                   ,(L2ArrayError x t1 (L2TX (L2Xvar newVar)))
                   ]
        (False,False) -> return [i]



genVar :: (Num a, Show a, MonadState a m) => L2Var -> m L2Var
genVar (L2Var name) = do
    num <- get
    put $ num + 1
    return $ L2Var $ name ++ show num

mkUpdate :: Int -> L2S -> L2Instruction
mkUpdate o s = (L2Update (L2Xreg (L2EBP)) o s)

mkRead :: L2X -> Int -> L2Instruction
mkRead x o = (L2ReadMem x (L2Xreg (L2EBP)) o)
