module L5ToL4.Compile where


import Control.Monad.State
import Data.List
import qualified Data.Map as M

import L5.Grammar
import L4.Grammar


translate :: L5Program -> L4Program
translate (L5Program e) = L4Program eOut gfs
    where
        process e = renamePass M.empty e >>= compileE
        (eOut,(CountFunState _ _ gfs)) = runCFS (process e) startCFS



data CountFunState = CountFunState { xCount :: Int
                                   , labCount :: Int
                                   , generatedFuns :: [L4Function]
                                   }
startCFS = CountFunState 0 0 []


type CFS a = State CountFunState a
runCFS = runState


{- Stateful functions -}
new4X = liftM prefix4X getIncXCount

prefix4X :: Int -> L4X
prefix4X n = L4X $ "x_" ++ show n

--
new5X = liftM prefix5X getIncXCount

prefix5X :: Int -> L5X
prefix5X n = L5X $ "v_" ++ show n

--
getIncXCount :: CFS Int
getIncXCount = do
    cfs@(CountFunState xc _ _) <- get
    put cfs{ xCount = xc + 1 }
    return xc


--
newLab = liftM prefixLab getIncLabCount

prefixLab :: Int -> L4Label
prefixLab n = L4Label $ "l_" ++ show n

getIncLabCount :: CFS Int
getIncLabCount = do
    cfs@(CountFunState _ lc _) <- get
    put cfs{ labCount = lc + 1 }
    return lc    


--
addFun :: L4Function -> CFS ()
addFun f = do
    cfs@(CountFunState _ _ gfs) <- get
    put cfs{ generatedFuns = (f:gfs) }



unpackLets :: L4X -> [L4X] -> (L4E -> L4E)
unpackLets tup_name xs = foldl (\ acc (x,idx) -> acc . (L4Let x (L4Aref (L4Ex tup_name) (L4Enum idx))))
                               id
                               (zip xs [0..])


compileE :: L5E -> CFS L4E
compileE (L5Lambda xs e) = do
    fun_lab <- newLab
    e_c <- compileE e
    let freeXs = map compileX $ findFree xs e
        convertedXs = (map compileX xs)
        extra_args = L4X "args"
        fun_args = if length convertedXs > 2
                      then [L4X "vars", (head convertedXs), extra_args]
                      else (L4X "vars":convertedXs)
        fun_body = if length convertedXs > 2
                      then unpackLets extra_args (tail convertedXs) $ e_c
                      else e_c
        vars_name = L4X "vars"
        fun = L4Function fun_lab fun_args (unpackLets vars_name freeXs $ fun_body)
    addFun fun
    return . L4MakeClosure fun_lab . L4NewTuple $ map L4Ex freeXs

compileE (L5Ex x) = return $ L4Ex (compileX x)

compileE (L5Let x e1 e2) = do
    [e1_c, e2_c] <- compileEs [e1,e2]
    return $ L4Let (compileX x) e1_c e2_c

compileE (L5LetRec x e1 e2) = do
    let x_c = compileX x
        xref = L5Apply (L5Primitive L5Aref) [(L5Ex x), (L5Enum 0)]
        e1_subbed = substituteE x xref e1
        e2_subbed = substituteE x xref e2
    [e1_c,e2_c] <- compileEs [e1_subbed,e2_subbed]
    return $ L4Let x_c (L4NewTuple [(L4Enum 0)])
                       (L4Begin (L4Aset (L4Ex x_c) (L4Enum 0) e1_c)
                                e2_c)

compileE (L5If tst thn els) = do
    [tst_c,thn_c,els_c] <- compileEs [tst,thn,els]
    return $ L4If tst_c thn_c els_c

compileE (L5NewTuple es) = do
    es_c <- compileEs es
    return $ L4NewTuple es_c

compileE (L5Begin e1 e2) = do
    [e1_c,e2_c] <- compileEs [e1,e2]
    return $ L4Begin e1_c e2_c

compileE (L5Apply f as) = case f of
    (L5Lambda xs e) -> do
        letStack <- foldM (\ acc (x,a) -> do
                                    a_c <- compileE a
                                    return $ acc . (L4Let (compileX x) a_c))
                          id
                          (zip xs as)
        e_c <- compileE e
        return $ letStack e_c
    (L5Primitive p) -> case p of
        L5Print -> do
            arg_c <- compileE (as !! 0)
            return $ L4Print arg_c
        L5NewArray -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4NewArray a1_c a2_c
        L5Aref -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4Aref a1_c a2_c
        L5Aset -> do
            [a1_c, a2_c, a3_c] <- compileEs $ take 3 as
            return $ L4Aset a1_c a2_c a3_c
        L5Alen -> do
            a_c <- compileE (as !! 0)
            return $ L4Alen a_c
        L5pb bop -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4Binop (compileBiopHead bop) a1_c a2_c
        L5pp p -> do
            a1_c <- compileE (as !! 0)
            return $ L4Predicate (compilePredHead p) a1_c
    _ -> do
        f_c <- compileE f
        as_c <- compileEs as
        let clos_proc = L4ClosureProc f_c
            clos_vars = L4ClosureVars f_c
            fun_args = if length as > 2
                          then [clos_vars, (head as_c), L4NewTuple (tail as_c)]
                          else (clos_vars:as_c)
        return $ L4Apply clos_proc fun_args

compileE (L5Primitive p) = case p of
    L5Print -> oneArityLambda L5Print
    L5NewArray -> twoArityLambda L5NewArray
    L5Aref -> twoArityLambda L5Aref
    L5Aset -> threeArityLambda L5Aset
    L5Alen -> oneArityLambda L5Alen
    L5pb bop -> twoArityLambda (L5pb bop)
    L5pp pd -> oneArityLambda (L5pp pd)

compileE (L5Enum n) = return $ L4Enum n



oneArityLambda p = let a1 = (L5X "x")
                   in compileE (L5Lambda [a1] (L5Apply (L5Primitive p) (map L5Ex [a1])))
twoArityLambda p = let a1 = (L5X "x")
                       a2 = (L5X "y")
                   in compileE (L5Lambda [a1,a2] (L5Apply (L5Primitive p) (map L5Ex [a1,a2])))
threeArityLambda p = let a1 = (L5X "x")
                         a2 = (L5X "y")
                         a3 = (L5X "z")
                     in compileE (L5Lambda [a1,a2,a3] (L5Apply (L5Primitive p) (map L5Ex [a1,a2,a3])))





substituteE :: L5X -> L5E -> L5E -> L5E
substituteE t r e@(L5Lambda xs body) = if t `elem` xs
                                       then e
                                       else singleSub (L5Lambda xs) (substituteE t r) body
substituteE t r e@(L5Ex x) = if x == t
                                then r
                                else e
substituteE t r e@(L5Let x e1 e2) = if x == t
                                       then singleSub (\ v -> L5Let x v e2) (substituteE t r) e1
                                       else doubleSub (L5Let x) (substituteE t r) e1 e2
substituteE t r e@(L5LetRec x e1 e2) = if x == t
                                          then e
                                          else doubleSub (L5LetRec x) (substituteE t r) e1 e2
substituteE t r (L5If e1 e2 e3) = tripleSub L5If (substituteE t r) e1 e2 e3
substituteE t r (L5NewTuple es) = L5NewTuple $ map (substituteE t r) es
substituteE t r (L5Begin e1 e2) = doubleSub L5Begin (substituteE t r) e1 e2
substituteE t r (L5Apply f as) = L5Apply (substituteE t r f) $ map (substituteE t r) as
substituteE t r e = e

singleSub f subF e1 = f (subF e1)
doubleSub f subF e1 e2 = f (subF e1) (subF e2)
tripleSub f subF e1 e2 e3 = f (subF e1) (subF e2) (subF e3)


findFree :: [L5X] -> L5E -> [L5X]
findFree bound (L5Lambda args e) = findFree (union bound args) e
findFree bound (L5Ex x) = [x] \\ bound
findFree bound (L5Let x e1 e2) = (findFree bound e1) ++ (findFree (union [x] bound) e2)
findFree bound (L5LetRec x e1 e2) = let newBound = (union [x] bound)
                                    in concatMap (findFree newBound) [e1,e2]
findFree bound (L5If e1 e2 e3) = concatMap (findFree bound) [e1,e2,e3]
findFree bound (L5NewTuple es) = concatMap (findFree bound) es
findFree bound (L5Begin e1 e2) = concatMap (findFree bound) [e1,e2]
findFree bound (L5Apply f as) = concatMap (findFree bound) (f:as)
findFree bound _ = []

compileEs :: [L5E] -> CFS [L4E]
compileEs = mapM compileE


compileBiopHead :: L5biop -> L4biop
compileBiopHead L5Add = L4Add
compileBiopHead L5Sub = L4Sub
compileBiopHead L5Mult = L4Mult
compileBiopHead L5LT = L4bcmp L4LessThan
compileBiopHead L5LTE = L4bcmp L4LessThanEqual
compileBiopHead L5Eq = L4bcmp L4Equal

compilePredHead :: L5pred -> L4pred
compilePredHead L5IsNumber = L4IsNumber
compilePredHead L5IsA = L4IsA


compileX :: L5X -> L4X
compileX (L5X name) = L4X name


{- Rename let-bound variables -}
renamePass :: M.Map L5X L5X -> L5E -> CFS L5E
renamePass env (L5Lambda xs e) = liftM (L5Lambda xs) (renamePass newEnv e)
    where
        newEnv = foldl (\ acc x -> M.delete x acc) env xs

renamePass env e@(L5Ex x) = case M.lookup x env of
    Nothing -> return e
    Just xN -> return $ L5Ex xN

renamePass env (L5Let x d b) = do
    xN <- new5X
    let newEnv = M.insert x xN env
    d_r <- renamePass env d
    b_r <- renamePass newEnv b
    return $ L5Let xN d_r b_r

renamePass env (L5LetRec x d b) = do
    xN <- new5X
    let newEnv = M.insert x xN env
    [d_r,b_r] <- mapM (renamePass newEnv) [d,b]
    return $ L5LetRec xN d_r b_r

renamePass env (L5If tst thn els) = do
    [tst_r,thn_r,els_r] <- mapM (renamePass env) [tst,thn,els]
    return $ L5If tst_r thn_r els_r

renamePass env (L5NewTuple es) = do
    es_r <- mapM (renamePass env) es
    return $ L5NewTuple es_r

renamePass env (L5Begin e1 e2) = do
    [e1_r,e2_r] <- mapM (renamePass env) [e1,e2]
    return $ L5Begin e1_r e2_r

renamePass env (L5Apply f as) = do
    (f_r:as_r) <- mapM (renamePass env) (f:as)
    return $ L5Apply f_r as_r

renamePass env e = return e




