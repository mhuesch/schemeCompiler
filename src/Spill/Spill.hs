module Spill.Spill where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import L2.AbsL2


spill :: [Instruction] -> Variable -> Int -> Variable -> [Instruction]
spill ls var offset prefix = res
    where (res, _) = runReadState (var, offset, prefix) 0 (spillInstructions ls)


type RS a = ReaderT Env (StateT Int Identity) a
type Env = (Variable, Int, Variable)

runReadState :: Env -> Int -> RS a -> (a, Int)
runReadState env st rs = runIdentity (runStateT (runReaderT rs env) st)


spillInstructions :: [Instruction] -> RS [Instruction]
spillInstructions ls = liftM concat $ mapM spillInstruction ls

variableToX :: Variable -> X
variableToX = Xw . Wcx . Var

variableToW :: Variable -> W
variableToW = Wcx . Var

spillInstruction :: Instruction -> RS [Instruction]
spillInstruction i@(IAssign w s) = do
    (var,varOffset,_) <- ask
    case ((w == variableToW var),(s == Sx (variableToX var))) of
        (True,True) -> return []
        (True,False) -> return [mkWrite varOffset s]
        (False,True) -> return [mkRead w varOffset]
        (False,False) -> return [i]

spillInstruction i@(IReadMem w x readOffset) = do
    (var,varOffset,prefix) <- ask
    case ((w == variableToW var),(x == variableToX var)) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IReadMem (variableToW newVar) (variableToX newVar) readOffset)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(IReadMem (variableToW newVar) x readOffset)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IReadMem w (variableToX newVar) readOffset)]
        (False,False) -> return [i]

spillInstruction i@(IWriteMem x updateOffset s) = do
    (var,varOffset,prefix) <- ask
    case ((x == variableToX var),(s == Sx (variableToX var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IWriteMem (variableToX newVar) updateOffset (Sx (variableToX newVar)))
                   ]
        (True, False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IWriteMem (variableToX newVar) updateOffset s)
                   ]
        (False, True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IWriteMem x updateOffset (Sx (variableToX newVar)))
                   ]
        (False, False) -> return [i]

spillInstruction i@(IArith w aop t) = do
    (var,varOffset,prefix) <- ask
    case ((w == variableToW var),(t == Tx (variableToX var))) of
        (True, True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IArith (variableToW newVar) aop (Tx (variableToX newVar)))
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True, False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IArith (variableToW newVar) aop t)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (False, True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IArith w aop (Tx (variableToX newVar)))
                   ]
        (False, False) -> return [i]

spillInstruction i@(IShiftCX w sop cx) = do
    (var,varOffset,prefix) <- ask
    case ((w == variableToW var),(cx == Var var)) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IShiftCX (variableToW newVar) sop (Var newVar))
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IShiftCX (variableToW newVar) sop cx)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IShiftCX w sop (Var newVar))
                   ]
        (False,False) -> return [i]

spillInstruction i@(IShiftN w sop n) = do
    (var,varOffset,prefix) <- ask
    case (w == variableToW var) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(IShiftN (variableToW newVar) sop n)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        False -> return [i]

spillInstruction i@(ISaveCmp w t1 cmp t2) = do
    (var,varOffset,prefix) <- ask
    case ((w == variableToW var),(t1 == (Tx (variableToX var))),(t2 == (Tx (variableToX var)))) of
        (True,True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp (variableToW newVar) (Tx (variableToX newVar)) cmp (Tx (variableToX newVar)))
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True,True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp (variableToW newVar) (Tx (variableToX newVar)) cmp t2)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True,False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp (variableToW newVar) t1 cmp (Tx (variableToX newVar)))
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (True,False,False) -> do
            newVar <- genVar prefix
            return [(ISaveCmp (variableToW newVar) t1 cmp t2)
                   ,(mkWrite varOffset (Sx (variableToX newVar)))
                   ]
        (False,True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp w (Tx (variableToX newVar)) cmp (Tx (variableToX newVar)))
                   ] 
        (False,True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp w (Tx (variableToX newVar)) cmp t2)
                   ] 
        (False,False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ISaveCmp w t1 cmp (Tx (variableToX newVar)))
                   ]
        (False,False,False) -> return [i]

spillInstruction i@(ILabel _) = return [i]
spillInstruction i@(IGoto _) = return [i]

spillInstruction i@(ICjump t1 cmp t2 l1 l2) = do
    (var,varOffset,prefix) <- ask
    case ((t1 == Tx (variableToX var)),(t2 == Tx (variableToX var))) of
        (True,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ICjump (Tx (variableToX newVar)) cmp (Tx (variableToX newVar)) l1 l2)
                   ]
        (True,False) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ICjump (Tx (variableToX newVar)) cmp t2 l1 l2)
                   ]
        (False,True) -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ICjump t1 cmp (Tx (variableToX newVar)) l1 l2)
                   ]
        (False,False) -> return [i]

spillInstruction i@(ICallNative u arity) = do
    (var,varOffset,prefix) <- ask
    case (u == Ux (variableToX var)) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ICallNative (Ux (variableToX newVar)) arity)
                   ]
        False -> return [i]

spillInstruction i@ICallRuntime{} = return [i]

spillInstruction i@(ITailCall u arity) = do
    (var,varOffset,prefix) <- ask
    case (u == Ux (variableToX var)) of
        True -> do
            newVar <- genVar prefix
            return [(mkRead (variableToW newVar) varOffset)
                   ,(ITailCall (Ux (variableToX newVar)) arity)
                   ]
        False -> return [i]

spillInstruction i@IReturn = return [i]


genVar :: (Num a, Show a, MonadState a m) => Variable -> m Variable
genVar (Variable name) = do
    num <- get
    put $ num + 1
    return $ Variable $ name ++ show num

intToN :: Int -> N
intToN = Num . PosNegInteger . show

mkWrite :: Int -> S -> Instruction
mkWrite o = IWriteMem RSP (intToN o)

mkRead :: W -> Int -> Instruction
mkRead w o = IReadMem w RSP (intToN o)
