module L4ToL3.Compile where


import Control.Monad.State
import qualified Data.Map as M

import qualified L4.AbsL4 as L4
import qualified L3.AbsL3 as L3

translate :: L4.Program -> L3.Program
translate (L4.Prog e fs) = (L3.Prog (compileE e)
                                    (map compileFunction fs))



startCS = CountState 0

data CountState = CountState { xCount :: Int
                             }

type CS a = State CountState a
runCS = runState



{- Statefully generate new variable -}
new3X = liftM prefix3X getIncXCount
new4X = liftM prefix4X getIncXCount

prefix3X :: Int -> L3.X
prefix3X n = L3.Var . L3.Variable $ "x_" ++ show n

prefix4X :: Int -> L4.X
prefix4X n = L4.Var . L4.Variable $ "x_" ++ show n


getIncXCount = do
    cs@(CountState xc) <- get
    put cs{ xCount = xc + 1}
    return xc




compileFunction :: L4.Function -> L3.Function
compileFunction (L4.Fun lab xs e) = (L3.Fun (compileLabel lab)
                                            (map compileX xs)
                                            (compileE e))


compileE :: L4.E -> L3.E
compileE e = fst $ runCS (process e) startCS
    where
        process x = (return x) >>= renamePass M.empty >>= norm


renamePass :: M.Map L4.X L4.X -> L4.E -> CS L4.E
renamePass env (L4.Let x e1 e2) = do
    xN <- new4X
    let newEnv = (M.insert x xN env)
    e1_r <- renamePass env e1
    e2_r <- renamePass newEnv e2
    return $ L4.Let xN e1_r e2_r

renamePass env (L4.If e1 e2 e3) = tripleERename env e1 e2 e3 L4.If

renamePass env (L4.Apply e es) = do
    (e1_r:es_r) <- mapM (renamePass env) (e:es)
    return $ L4.Apply e1_r es_r

renamePass env (L4.NewArray e1 e2) = doubleERename env e1 e2 L4.NewArray

renamePass env (L4.NewTuple es) = do
    es_r <- mapM (renamePass env) es
    return $ L4.NewTuple es_r

renamePass env (L4.Aref e1 e2) = doubleERename env e1 e2 L4.Aref

renamePass env (L4.Aset e1 e2 e3) = tripleERename env e1 e2 e3 L4.Aset

renamePass env (L4.Alen e1) = singleERename env e1 L4.Alen

renamePass env (L4.Begin e1 e2) = doubleERename env e1 e2 L4.Begin

renamePass env (L4.Print e1) = singleERename env e1 L4.Print

renamePass env (L4.MakeClosure lab e1) = singleERename env e1 (L4.MakeClosure lab)

renamePass env (L4.ClosureProc e1) = singleERename env e1 L4.ClosureProc

renamePass env (L4.ClosureVars e1) = singleERename env e1 L4.ClosureVars

renamePass env (L4.Binop bop e1 e2) = doubleERename env e1 e2 (L4.Binop bop)

renamePass env (L4.Predicate p e1) = singleERename env e1 (L4.Predicate p)

renamePass env e@(L4.Ev v) = case v of
                               L4.Vx x -> case M.lookup x env of
                                            Just xN -> return . L4.Ev . L4.Vx $ xN
                                            _ -> return e
                               _ -> return e

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

data Context = LetCtxt L4.X L4.E Context
             | IfCtxt L4.E L4.E Context
             | FunCtxt [L4.E] Context
             | ArgCtxt L4.E [L4.E] [L4.E] Context
             | PrintCtxt Context
             | Binop1Ctxt L4.Biop L4.E Context
             | Binop2Ctxt L4.Biop L4.E Context
             | PredCtxt L4.Pred Context
             | NewTupleCtxt [L4.E] [L4.E] Context
             | NewArr1Ctxt L4.E Context
             | NewArr2Ctxt L4.E Context
             | Aref1Ctxt L4.E Context
             | Aref2Ctxt L4.E Context
             | Aset1Ctxt L4.E L4.E Context
             | Aset2Ctxt L4.E L4.E Context
             | Aset3Ctxt L4.E L4.E Context
             | AlenCtxt Context
             | MakeClosCtxt L4.Label Context
             | ClosProcCtxt Context
             | ClosVarsCtxt Context
             | NoCtxt


norm :: L4.E -> CS L3.E
norm e = find e NoCtxt

find :: L4.E -> Context -> CS L3.E
find e k = case e of
    (L4.Let x r b) -> find r (LetCtxt x b k)
    (L4.If tst thn els) -> find tst (IfCtxt thn els k)
    (L4.Apply f args) -> find f (FunCtxt args k)
    (L4.Print e') -> find e' (PrintCtxt k)
    (L4.Binop bop e1 e2) -> find e1 (Binop1Ctxt bop e2 k)
    (L4.Predicate p e') -> find e' (PredCtxt p k)
    (L4.NewTuple (e':es)) -> find e' (NewTupleCtxt [] es k)
    (L4.NewArray e1 e2) -> find e1 (NewArr1Ctxt e2 k)
    (L4.Aref e1 e2) -> find e1 (Aref1Ctxt e2 k)
    (L4.Aset e1 e2 e3) -> find e1 (Aset1Ctxt e2 e3 k)
    (L4.Alen e') -> find e' (AlenCtxt k)
    (L4.Begin e1 e2) -> do
        x <- new4X
        find (L4.Let x e1 e2) k
    (L4.MakeClosure lab e') -> find e' (MakeClosCtxt lab k)
    (L4.ClosureProc e') -> find e' (ClosProcCtxt k)
    (L4.ClosureVars e') -> find e' (ClosVarsCtxt k)
    _ -> fill e k




fill :: L4.E -> Context -> CS L3.E
fill d k = case k of
    (LetCtxt x b k2) -> do
        d_res <- find b k2
        return $  L3.Let (compileX x) (coerceEtoD d) d_res
    (IfCtxt thn els k2) -> case (isVal d) of
        True -> do
            thn_res <- find thn k2
            els_res <- find els k2
            return $ L3.If (coerceEtoV d) thn_res els_res
        False -> do
            x <- new3X
            thn_res <- find thn k2
            els_res <- find els k2
            return $ L3.Let x (coerceEtoD d) $ L3.If (L3.Vx x) thn_res els_res

    (FunCtxt args k2) -> case args of
        [] -> maybeLet d (\v -> fill (L4.Apply v []) k2)
        (a:as) -> maybeLet d (\v -> find a (ArgCtxt v [] as k2))
    
    (ArgCtxt fv avs aes k2) -> case aes of
        [] -> maybeLet d (\v -> fill (L4.Apply fv (avs ++ [v])) k2)
        (a:as) -> maybeLet d (\v -> find a (ArgCtxt fv (avs ++ [v]) as k2))

    (PrintCtxt k2) -> maybeLet d (\v -> fill (L4.Print v) k2)

    (Binop1Ctxt bop e2 k2) -> maybeLet d (\v -> find e2 (Binop2Ctxt bop v k2))
    (Binop2Ctxt bop e1 k2) -> maybeLet d (\v -> fill (L4.Binop bop e1 v) k2)

    (PredCtxt p k2) -> maybeLet d (\v -> fill (L4.Predicate p v) k2)

    (NewTupleCtxt vs es k2) -> case es of
        [] -> maybeLet d (\v -> fill (L4.NewTuple (vs ++ [v])) k2)
        (x:xs) -> maybeLet d (\v -> find x (NewTupleCtxt (vs ++ [v]) xs k2))
    
    (NewArr1Ctxt e2 k2) -> maybeLet d (\v -> find e2 (NewArr2Ctxt v k2))
    (NewArr2Ctxt e1 k2) -> maybeLet d (\v -> fill (L4.NewArray e1 v) k2)

    (Aref1Ctxt e2 k2) -> maybeLet d (\v -> find e2 (Aref2Ctxt v k2))
    (Aref2Ctxt e1 k2) -> maybeLet d (\v -> fill (L4.Aref e1 v) k2)

    (Aset1Ctxt e2 e3 k2) -> maybeLet d (\v -> find e2 (Aset2Ctxt v e3 k2))
    (Aset2Ctxt e1 e3 k2) -> maybeLet d (\v -> find e3 (Aset3Ctxt e1 v k2))
    (Aset3Ctxt e1 e2 k2) -> maybeLet d (\v -> fill (L4.Aset e1 e2 v) k2)

    (AlenCtxt k2) -> maybeLet d (\v -> fill (L4.Alen v) k2)

    (MakeClosCtxt lab k2) -> maybeLet d (\v -> fill (L4.MakeClosure lab v) k2)

    (ClosProcCtxt k2) -> maybeLet d (\v -> fill (L4.ClosureProc v) k2)

    (ClosVarsCtxt k2) -> maybeLet d (\v -> fill (L4.ClosureVars v) k2)

    NoCtxt -> return $ coerceEtoE d


maybeLet d f = if (isVal d)
                  then f d
                  else do
                     x <- new4X
                     f_res <- f . L4.Ev . L4.Vx $ x
                     return $ L3.Let (compileX x) (coerceEtoD d) f_res


isVal :: L4.E -> Bool
isVal (L4.Ev _) = True
isVal _ = False

coerceEtoE :: L4.E -> L3.E
coerceEtoE (L4.Let x d b) = (L3.Let (compileX x) (coerceEtoD d) (coerceEtoE b))
coerceEtoE (L4.If tst thn els) = (L3.If (coerceEtoV tst) (coerceEtoE thn) (coerceEtoE els))
coerceEtoE e = L3.Ed . coerceEtoD $ e

coerceEtoD :: L4.E -> L3.D
coerceEtoD (L4.Binop bop e1 e2) = (L3.Binop (compileBiop bop) (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4.Predicate p e) = (L3.Predicate (compilePred p) (coerceEtoV e))
coerceEtoD (L4.Apply e es) = (L3.Apply (coerceEtoV e) (map coerceEtoV es))
coerceEtoD (L4.NewArray e1 e2) = (L3.NewArray (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4.NewTuple es) = (L3.NewTuple (map coerceEtoV es))
coerceEtoD (L4.Aref e1 e2) = (L3.Aref (coerceEtoV e1) (coerceEtoV e2))
coerceEtoD (L4.Aset e1 e2 e3) = (L3.Aset (coerceEtoV e1) (coerceEtoV e2) (coerceEtoV e3))
coerceEtoD (L4.Alen e) = (L3.Alen (coerceEtoV e))
coerceEtoD (L4.Print e) = (L3.Print (coerceEtoV e))
coerceEtoD (L4.MakeClosure lab e) = (L3.MakeClosure (compileLabel lab) (coerceEtoV e))
coerceEtoD (L4.ClosureProc e) = (L3.ClosureProc (coerceEtoV e))
coerceEtoD (L4.ClosureVars e) = (L3.ClosureVars (coerceEtoV e))
coerceEtoD e = L3.Dv . coerceEtoV $ e


coerceEtoV :: L4.E -> L3.V
coerceEtoV (L4.Ev v) = compileV v
coerceEtoV e = error $ "invalid: " ++ show e


compileLabel :: L4.Label -> L3.Label
compileLabel (L4.Label name) = L3.Label name

compileV :: L4.V -> L3.V
compileV (L4.Vx x) = L3.Vx $ compileX x
compileV (L4.Vlab l) = L3.Vlab $ compileLabel l
compileV (L4.Vnum n) = L3.Vnum $ compilePNI n

compileX :: L4.X -> L3.X
compileX (L4.Var var) = L3.Var $ compileVariable var

compileVariable :: L4.Variable -> L3.Variable 
compileVariable (L4.Variable s) = L3.Variable s

compilePNI :: L4.PosNegInteger -> L3.PosNegInteger
compilePNI (L4.PosNegInteger s) = L3.PosNegInteger s

compileBiop :: L4.Biop -> L3.Biop
compileBiop L4.Add = L3.Add
compileBiop L4.Sub = L3.Sub
compileBiop L4.Mult = L3.Mult
compileBiop L4.LessThan = L3.LessThan
compileBiop L4.LessThanEqual = L3.LessThanEqual
compileBiop L4.Equal = L3.Equal

compilePred :: L4.Pred -> L3.Pred
compilePred L4.IsNum = L3.IsNum
compilePred L4.IsA = L3.IsA

