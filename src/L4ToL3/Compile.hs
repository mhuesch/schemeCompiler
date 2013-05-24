module L4ToL3.Compile where


import Control.Monad.State
import qualified Data.Map as M

import L4.Grammar
import L3.Grammar

translate :: L4Program -> L3Program
translate (L4Program e fs) = (L3Program (compileE e)
                                        (map compileFunction fs))



startCS = CountState 0

data CountState = CountState { xCount :: Int
                             }

type CS a = State CountState a
runCS = runState



{- Statefully generate new variable -}
new3X = liftM prefix3X getIncXCount
new4X = liftM prefix4X getIncXCount

prefix3X :: Int -> L3X
prefix3X n = L3X $ "x_" ++ show n

prefix4X :: Int -> L4X
prefix4X n = L4X $ "x_" ++ show n


getIncXCount = do
    cs@(CountState xc) <- get
    put cs{ xCount = xc + 1}
    return xc




compileFunction :: L4Function -> L3Function
compileFunction (L4Function lab xs e) = (L3Function (compileLabel lab)
                                                    (map compileX xs)
                                                    (compileE e))


compileE :: L4E -> L3E
compileE e = fst $ runCS (process e) startCS
    where
        process x = (return x) >>= renamePass M.empty >>= norm


renamePass :: M.Map L4X L4X -> L4E -> CS L4E
renamePass env (L4Let x e1 e2) = do
    xN <- new4X
    let newEnv = (M.insert x xN env)
    e1_r <- renamePass env e1
    e2_r <- renamePass newEnv e2
    return $ L4Let xN e1_r e2_r

renamePass env (L4If e1 e2 e3) = tripleERename env e1 e2 e3 L4If

renamePass env (L4Apply e es) = do
    (e1_r:es_r) <- mapM (renamePass env) (e:es)
    return $ L4Apply e1_r es_r

renamePass env (L4NewArray e1 e2) = doubleERename env e1 e2 L4NewArray

renamePass env (L4NewTuple es) = do
    es_r <- mapM (renamePass env) es
    return $ L4NewTuple es_r

renamePass env (L4Aref e1 e2) = doubleERename env e1 e2 L4Aref

renamePass env (L4Aset e1 e2 e3) = tripleERename env e1 e2 e3 L4Aset

renamePass env (L4Alen e1) = singleERename env e1 L4Alen

renamePass env (L4Begin e1 e2) = doubleERename env e1 e2 L4Begin

renamePass env (L4Print e1) = singleERename env e1 L4Print

renamePass env (L4MakeClosure lab e1) = singleERename env e1 (L4MakeClosure lab)

renamePass env (L4ClosureProc e1) = singleERename env e1 L4ClosureProc

renamePass env (L4ClosureVars e1) = singleERename env e1 L4ClosureVars

renamePass env (L4Binop bop e1 e2) = doubleERename env e1 e2 (L4Binop bop)

renamePass env (L4Predicate p e1) = singleERename env e1 (L4Predicate p)

renamePass env e@(L4Enum _) = return e

renamePass env e@(L4Ex x) = case M.lookup x env of
    Nothing -> return e
    Just xN -> return $ L4Ex xN

renamePass env e@(L4Elab _) = return e

------

singleERename env e1 f = do
    e1_r <- renamePass env e1
    return $ f e1_r

doubleERename env e1 e2 f = do
    [e1_r,e2_r] <- mapM (renamePass env) [e1,e2]
    return $ f e1_r e2_r

tripleERename env e1 e2 e3 f = do
    [e1_r,e2_r,e3_r] <- mapM (renamePass env) [e1,e2,e3]
    return $ f e1_r e2_r e3_r

data Context = LetCtxt L4X L4E Context
             | IfCtxt L4E L4E Context
             | FunCtxt [L4E] Context
             | ArgCtxt L4E [L4E] [L4E] Context
             | PrintCtxt Context
             | Binop1Ctxt L4biop L4E Context
             | Binop2Ctxt L4biop L4E Context
             | PredCtxt L4pred Context
             | NewTupleCtxt [L4E] [L4E] Context
             | NewArr1Ctxt L4E Context
             | NewArr2Ctxt L4E Context
             | Aref1Ctxt L4E Context
             | Aref2Ctxt L4E Context
             | Aset1Ctxt L4E L4E Context
             | Aset2Ctxt L4E L4E Context
             | Aset3Ctxt L4E L4E Context
             | AlenCtxt Context
             | MakeClosCtxt L4Label Context
             | ClosProcCtxt Context
             | ClosVarsCtxt Context
             | NoCtxt


norm :: L4E -> CS L3E
norm e = find e NoCtxt

find :: L4E -> Context -> CS L3E
find e k = case e of
    (L4Let x r b) -> find r (LetCtxt x b k)
    (L4If tst thn els) -> find tst (IfCtxt thn els k)
    (L4Apply f args) -> find f (FunCtxt args k)
    (L4Print e) -> find e (PrintCtxt k)
    (L4Binop bop e1 e2) -> find e1 (Binop1Ctxt bop e2 k)
    (L4Predicate p e) -> find e (PredCtxt p k)
    (L4NewTuple (e:es)) -> find e (NewTupleCtxt [] es k)
    (L4NewArray e1 e2) -> find e1 (NewArr1Ctxt e2 k)
    (L4Aref e1 e2) -> find e1 (Aref1Ctxt e2 k)
    (L4Aset e1 e2 e3) -> find e1 (Aset1Ctxt e2 e3 k)
    (L4Alen e) -> find e (AlenCtxt k)
    (L4Begin e1 e2) -> do
        x <- new4X
        find (L4Let x e1 e2) k
    (L4MakeClosure lab e) -> find e (MakeClosCtxt lab k)
    (L4ClosureProc e) -> find e (ClosProcCtxt k)
    (L4ClosureVars e) -> find e (ClosVarsCtxt k)
    _ -> fill e k




fill :: L4E -> Context -> CS L3E
fill d k = case k of
    (LetCtxt x b k2) -> do
        d_res <- find b k2
        return $  L3Let (compileX x) (coerceEtoD d) d_res
    (IfCtxt thn els k2) -> case (isVal d) of
        True -> do
            thn_res <- find thn k2
            els_res <- find els k2
            return $ L3If (coerceEtoV d) thn_res els_res
        False -> do
            x <- new3X
            thn_res <- find thn k2
            els_res <- find els k2
            return $ L3Let x (coerceEtoD d) $ L3If (L3Vx x) thn_res els_res

    (FunCtxt args k2) -> case args of
        [] -> maybeLet d (\v -> fill (L4Apply v []) k2)
        (a:as) -> maybeLet d (\v -> find a (ArgCtxt v [] as k2))
    
    (ArgCtxt fv avs aes k2) -> case aes of
        [] -> maybeLet d (\v -> fill (L4Apply fv (avs ++ [v])) k2)
        (a:as) -> maybeLet d (\v -> find a (ArgCtxt fv (avs ++ [v]) as k2))

    (PrintCtxt k2) -> maybeLet d (\v -> fill (L4Print v) k2)

    (Binop1Ctxt bop e2 k2) -> maybeLet d (\v -> find e2 (Binop2Ctxt bop v k2))
    (Binop2Ctxt bop e1 k2) -> maybeLet d (\v -> fill (L4Binop bop e1 v) k2)

    (PredCtxt p k2) -> maybeLet d (\v -> fill (L4Predicate p v) k2)

    (NewTupleCtxt vs es k2) -> case es of
        [] -> maybeLet d (\v -> fill (L4NewTuple (vs ++ [v])) k2)
        (x:xs) -> maybeLet d (\v -> find x (NewTupleCtxt (vs ++ [v]) xs k2))
    
    (NewArr1Ctxt e2 k2) -> maybeLet d (\v -> find e2 (NewArr2Ctxt v k2))
    (NewArr2Ctxt e1 k2) -> maybeLet d (\v -> fill (L4NewArray e1 v) k2)

    (Aref1Ctxt e2 k2) -> maybeLet d (\v -> find e2 (Aref2Ctxt v k2))
    (Aref2Ctxt e1 k2) -> maybeLet d (\v -> fill (L4Aref e1 v) k2)

    (Aset1Ctxt e2 e3 k2) -> maybeLet d (\v -> find e2 (Aset2Ctxt v e3 k2))
    (Aset2Ctxt e1 e3 k2) -> maybeLet d (\v -> find e3 (Aset3Ctxt e1 v k2))
    (Aset3Ctxt e1 e2 k2) -> maybeLet d (\v -> fill (L4Aset e1 e2 v) k2)

    (AlenCtxt k2) -> maybeLet d (\v -> fill (L4Alen v) k2)

    (MakeClosCtxt lab k2) -> maybeLet d (\v -> fill (L4MakeClosure lab v) k2)

    (ClosProcCtxt k2) -> maybeLet d (\v -> fill (L4ClosureProc v) k2)

    (ClosVarsCtxt k2) -> maybeLet d (\v -> fill (L4ClosureVars v) k2)

    NoCtxt -> return $ coerceEtoE d


maybeLet d f = if (isVal d)
                  then f d
                  else do
                     x <- new4X
                     f_res <- f (L4Ex x)
                     return $ L3Let (compileX x) (coerceEtoD d) f_res


isVal :: L4E -> Bool
isVal (L4Enum _) = True
isVal (L4Ex _) = True
isVal (L4Elab _) = True
isVal _ = False

coerceEtoE :: L4E -> L3E
coerceEtoE (L4Let x d b) = (L3Let (compileX x) (coerceEtoD d) (coerceEtoE b))
coerceEtoE (L4If tst thn els) = (L3If (coerceEtoV tst) (coerceEtoE thn) (coerceEtoE els))
coerceEtoE e = L3Ed . coerceEtoD $ e

coerceEtoD :: L4E -> L3D
coerceEtoD (L4Binop bop e1 e2) = (L3Binop (compileBiop bop) (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4Predicate p e) = (L3Predicate (compilePred p) (coerceEtoV e))
coerceEtoD (L4Apply e es) = (L3Apply (coerceEtoV e) (map coerceEtoV es))
coerceEtoD (L4NewArray e1 e2) = (L3NewArray (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4NewTuple es) = (L3NewTuple (map coerceEtoV es))
coerceEtoD (L4Aref e1 e2) = (L3Aref (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4Aset e1 e2 e3) = (L3Aset (coerceEtoV e1) (coerceEtoV e2) (coerceEtoV e3))
coerceEtoD (L4Alen e) = (L3Alen (coerceEtoV e))
coerceEtoD (L4Print e) = (L3Print (coerceEtoV e))
coerceEtoD (L4MakeClosure lab e) = (L3MakeClosure (compileLabel lab) (coerceEtoV e))
coerceEtoD (L4ClosureProc e) = (L3ClosureProc (coerceEtoV e))
coerceEtoD (L4ClosureVars e) = (L3ClosureVars (coerceEtoV e))
coerceEtoD e = L3Dv . coerceEtoV $ e


coerceEtoV :: L4E -> L3V
coerceEtoV (L4Enum n) = L3Vnum n
coerceEtoV (L4Elab l) = L3Vlab (compileLabel l)
coerceEtoV (L4Ex x) = L3Vx (compileX x)
coerceEtoV e = error $ "invalid: " ++ show e


compileLabel :: L4Label -> L3Label
compileLabel (L4Label name) = L3Label name

compileX :: L4X -> L3X
compileX (L4X name) = L3X name

compileBiop :: L4biop -> L3biop
compileBiop L4Add = L3Add
compileBiop L4Sub = L3Sub
compileBiop L4Mult = L3Mult
compileBiop (L4bcmp L4LessThan) = L3LessThan
compileBiop (L4bcmp L4LessThanEqual) = L3LessThanEqual
compileBiop (L4bcmp L4Equal) = L3Equal

compilePred :: L4pred -> L3pred
compilePred L4IsNumber = L3IsNumber
compilePred L4IsA = L3IsA

