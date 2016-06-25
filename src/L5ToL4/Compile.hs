module L5ToL4.Compile where


import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import qualified Data.Map as M

import qualified L5.AbsL5 as L5
import qualified L4.AbsL4 as L4

data CountFunState = CountFunState { xCount :: Int
                                   , labCount :: Int
                                   , generatedFuns :: [L4.Function]
                                   }

translate :: L5.Program -> L4.Program
translate (L5.Prog e) = L4.Prog eOut (generatedFuns cfs)
    where
        process e' = renamePass M.empty e' >>= compileE
        (eOut,cfs) = runCFS startCFS M.empty (process e)


startCFS :: CountFunState
startCFS = CountFunState 0 0 []


-- ReaderT maps from L5.X's (that functions are bound to) to the
-- corresponding L4. function label and the arguments of the function
--
-- StateT holds a CountFunState, which keeps track of x & label counts
-- (so fresh ones are generated) and holds the L4. Functions that are
-- generated as we talk over the L5. program tree
type CFS a = ReaderT (M.Map L5.X (L4.Label,[L5.X])) (StateT CountFunState Identity) a
runCFS :: s -> r -> ReaderT r (StateT s Identity) a -> (a, s)
runCFS st env comp = runIdentity (runStateT (runReaderT comp env) st)


--
-- Misc helpers
--
make4X :: String -> L4.X
make4X = L4.Var . L4.Variable

make5X :: String -> L5.X
make5X = L5.Var . L5.Variable

make4PNI :: Int -> L4.PosNegInteger
make4PNI = L4.PosNegInteger . show

make5PNI :: Int -> L5.PosNegInteger
make5PNI = L5.PosNegInteger . show

{- Stateful functions -}
new4X :: CFS L4.X
new4X = liftM prefix4X getIncXCount

prefix4X :: Int -> L4.X
prefix4X n = make4X $ "x_" ++ show n
--
--
new5X :: CFS L5.X
new5X = liftM prefix5X getIncXCount

prefix5X :: Int -> L5.X
prefix5X n = make5X $ "v_" ++ show n

getIncXCount :: CFS Int
--getIncXCount = xCount <+= 1
getIncXCount = do
    cfs@(CountFunState xc _ _) <- get
    put cfs{ xCount = xc + 1 }
    return xc
--
--
newLab :: CFS L4.Label
newLab = liftM prefixLab getIncLabCount

prefixLab :: Int -> L4.Label
prefixLab n = L4.Label $ ":f_" ++ show n

getIncLabCount :: CFS Int
--getIncLabCount = labCount <+= 1
getIncLabCount = do
    cfs@(CountFunState _ lc _) <- get
    put cfs{ labCount = lc + 1 }
    return lc
--
--
addFun :: L4.Function -> CFS ()
--addFun f = generatedFuns %= (f:)
addFun f = do
    cfs@(CountFunState _ _ gfs) <- get
    put cfs{ generatedFuns = (f:gfs) }
--

unpackLets :: L4.X -> [L4.X] -> L4.E -> L4.E
unpackLets tup_name xs = foldl (\ acc (x,idx) -> acc . (L4.Let x (L4.Aref (L4.Ev $ L4.Vx tup_name) (L4.Ev . L4.Vnum . make4PNI $ idx))))
                               id
                               (zip xs [0..])


compileE :: L5.E -> CFS L4.E
compileE (L5.Lambda xs e) = do
    env <- ask
    e_c <- compileE e
    let freeXs = map compileX $ (findFreeNonDuplicates xs e) \\ M.keys env
        compiledArgs = (map compileX xs)
        extra_args = make4X "args"
        fun_args = if length compiledArgs > 2
                      then [make4X "vars", (head compiledArgs), extra_args]
                      else make4X "vars":compiledArgs
        fun_body = if length compiledArgs > 2
                      then unpackLets extra_args (tail compiledArgs) e_c
                      else e_c
        vars_name = make4X "vars"
    fun_lab <- newLab
    addFun $ L4.Fun fun_lab fun_args (unpackLets vars_name freeXs fun_body)
    return . L4.MakeClosure fun_lab . L4.NewTuple $ map (L4.Ev . L4.Vx) freeXs

-- If x is in the ReaderT env, it means it is bound to a function which we
-- have optimized to have no closure. If we reach this case it means the
-- function will "escape" and be returned by a function. This would ambush
-- a potential caller because they won't know it doesn't have a closure, so
-- we compile it to have a closure here.
compileE (L5.Ex x) = do
    env <- ask
    case M.lookup x env of
        Nothing -> return . L4.Ev . L4.Vx . compileX $ x
        Just (_,args) -> compileE (L5.Lambda args (L5.Apply (L5.Ex x) (map L5.Ex args)))

compileE (L5.Let x e1 e2) = do
    [e1_c, e2_c] <- compileEs [e1,e2]
    return $ L4.Let (compileX x) e1_c e2_c

compileE (L5.LetRec x e1 e2) = case e1 of
    L5.Lambda args body -> if null (findFreeNonDuplicates (x:args) body)
                             -- Here we compile the function without
                             -- a closure, for efficiency reasons. We
                             -- insert it into the ReaderT env so we will
                             -- know later that it is closure-less.
                             then do
                                fun_lab <- newLab
                                body_c <- local (M.insert x (fun_lab,args)) $ compileE body
                                -- Do extra args here
                                let fun_args = map compileX args
                                    extra_args = make4X "args"
                                    compiledArgs = map compileX args
                                    packed_fun_args = if length fun_args > 3
                                                         then (take 2 fun_args) ++ [extra_args]
                                                         else fun_args
                                    unpack_extra_args = if length args > 3
                                                           then unpackLets extra_args $ drop 2 compiledArgs
                                                           else id
                                    fun_body = unpack_extra_args body_c
                                addFun $ L4.Fun fun_lab packed_fun_args fun_body
                                local (M.insert x (fun_lab,args)) $ compileE e2
                             else normalLetRec

    _ -> normalLetRec
    where normalLetRec = do
                let x_c = compileX x
                    xref = L5.Apply (L5.Eprim L5.Aref) [(L5.Ex x), (L5.Enum $ make5PNI 0)]
                    e1_subbed = substituteE x xref e1
                    e2_subbed = substituteE x xref e2
                [e1_c,e2_c] <- compileEs [e1_subbed,e2_subbed]
                return $ L4.Let x_c (L4.NewTuple [(L4.Ev $ L4.Vnum $ make4PNI 0)])
                                   (L4.Begin (L4.Aset (L4.Ev $ L4.Vx x_c) (L4.Ev . L4.Vnum . make4PNI $ 0) e1_c)
                                            e2_c)

compileE (L5.If tst thn els) = do
    [tst_c,thn_c,els_c] <- compileEs [tst,thn,els]
    return $ L4.If tst_c thn_c els_c

compileE (L5.NewTuple es) = do
    es_c <- compileEs es
    return $ L4.NewTuple es_c

compileE (L5.Begin e1 e2) = do
    [e1_c,e2_c] <- compileEs [e1,e2]
    return $ L4.Begin e1_c e2_c

compileE (L5.Apply f as) = case f of
    (L5.Lambda xs e) -> do
        letStack <- foldM (\ acc (x,a) -> do
                                    a_c <- compileE a
                                    return $ acc . (L4.Let (compileX x) a_c))
                          id
                          (zip xs as)
        e_c <- compileE e
        return $ letStack e_c
    (L5.Eprim p) -> case p of
        L5.Print -> do
            arg_c <- compileE (head as) 
            return $ L4.Print arg_c
        L5.NewArray -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4.NewArray a1_c a2_c
        L5.Aref -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4.Aref a1_c a2_c
        L5.Aset -> do
            [a1_c, a2_c, a3_c] <- compileEs $ take 3 as
            return $ L4.Aset a1_c a2_c a3_c
        L5.Alen -> do
            a_c <- compileE (head as) 
            return $ L4.Alen a_c
        L5.Pb bop -> do
            [a1_c, a2_c] <- compileEs $ take 2 as
            return $ L4.Binop (compileBiopHead bop) a1_c a2_c
        L5.Pp pred' -> do
            a1_c <- compileE (head as)
            return $ L4.Predicate (compilePredHead pred') a1_c
    (L5.Ex x) -> do
        env <- ask
        case M.lookup x env of
            Nothing -> genericApply
            Just (fun_lab,_) -> do
                as_c <- compileEs as
                let fun_args = if length as_c > 3
                                  then (take 2 as_c) ++ [L4.NewTuple (drop 2 as_c)]
                                  else as_c
                return $ L4.Apply (L4.Ev $ L4.Vlab fun_lab) fun_args
    _ -> genericApply
    where genericApply = do
                f_c <- compileE f
                as_c <- compileEs as
                clo_label <- new4X
                let clo_let = L4.Let clo_label f_c
                    clos_proc = L4.ClosureProc (L4.Ev $ L4.Vx clo_label)
                    clos_vars = L4.ClosureVars (L4.Ev $ L4.Vx clo_label)
                    fun_args = if length as > 2
                                  then [(head as_c), L4.NewTuple (tail as_c)]
                                  else as_c
                return $ clo_let $ L4.Apply clos_proc (clos_vars:fun_args)

compileE (L5.Eprim p) = case p of
    L5.Print -> oneArityLambda L5.Print
    L5.NewArray -> twoArityLambda L5.NewArray
    L5.Aref -> twoArityLambda L5.Aref
    L5.Aset -> threeArityLambda L5.Aset
    L5.Alen -> oneArityLambda L5.Alen
    L5.Pb bop -> twoArityLambda (L5.Pb bop)
    L5.Pp pd -> oneArityLambda (L5.Pp pd)

compileE (L5.Enum n) = return . L4.Ev . L4.Vnum . compilePNI $ n


compileEs :: [L5.E] -> CFS [L4.E]
compileEs = mapM compileE


-- Lambdas
genericArityLambda :: [L5.X] -> L5.Prim -> CFS L4.E
genericArityLambda as p = compileE (L5.Lambda as (L5.Apply (L5.Eprim p) (map L5.Ex as)))
--
oneArityLambda :: L5.Prim -> CFS L4.E
oneArityLambda = genericArityLambda [(make5X "x")]
twoArityLambda :: L5.Prim -> CFS L4.E
twoArityLambda = genericArityLambda [(make5X "x"), (make5X "y")]
threeArityLambda :: L5.Prim -> CFS L4.E
threeArityLambda = genericArityLambda [(make5X "x"), (make5X "y"), (make5X "z")]



substituteE :: L5.X -> L5.E -> L5.E -> L5.E
substituteE t r e@(L5.Lambda xs body) = if t `elem` xs
                                       then e
                                       else singleSub (L5.Lambda xs) (substituteE t r) body
substituteE t r e@(L5.Ex x) = if x == t
                                then r
                                else e
substituteE t r (L5.Let x e1 e2) = if x == t
                                       then singleSub (\ v -> L5.Let x v e2) (substituteE t r) e1
                                       else doubleSub (L5.Let x) (substituteE t r) e1 e2
substituteE t r e@(L5.LetRec x e1 e2) = if x == t
                                          then e
                                          else doubleSub (L5.LetRec x) (substituteE t r) e1 e2
substituteE t r (L5.If e1 e2 e3) = tripleSub L5.If (substituteE t r) e1 e2 e3
substituteE t r (L5.NewTuple es) = L5.NewTuple $ map (substituteE t r) es
substituteE t r (L5.Begin e1 e2) = doubleSub L5.Begin (substituteE t r) e1 e2
substituteE t r (L5.Apply f as) = L5.Apply (substituteE t r f) $ map (substituteE t r) as
substituteE _ _ e = e

singleSub ::  (t1 -> t) -> (t2 -> t1) -> t2 -> t
singleSub f subF e1 = f (subF e1)
doubleSub ::  (t1 -> t1 -> t) -> (t2 -> t1) -> t2 -> t2 -> t
doubleSub f subF e1 e2 = f (subF e1) (subF e2)
tripleSub :: (t1 -> t1 -> t1 -> t) -> (t2 -> t1) -> t2 -> t2 -> t2 -> t
tripleSub f subF e1 e2 e3 = f (subF e1) (subF e2) (subF e3)

findFreeNonDuplicates :: [L5.X] -> L5.E -> [L5.X]
findFreeNonDuplicates bound = nub . findFree bound

findFree :: [L5.X] -> L5.E -> [L5.X]
findFree bound (L5.Lambda args e) = findFree (union bound args) e
findFree bound (L5.Ex x) = [x] \\ bound
findFree bound (L5.Let x e1 e2) = (findFree bound e1) ++ (findFree (union [x] bound) e2)
findFree bound (L5.LetRec x e1 e2) = let newBound = (union [x] bound)
                                     in concatMap (findFree newBound) [e1,e2]
findFree bound (L5.If e1 e2 e3) = concatMap (findFree bound) [e1,e2,e3]
findFree bound (L5.NewTuple es) = concatMap (findFree bound) es
findFree bound (L5.Begin e1 e2) = concatMap (findFree bound) [e1,e2]
findFree bound (L5.Apply f as) = concatMap (findFree bound) (f:as)
findFree _ _ = []



compileBiopHead :: L5.Biop -> L4.Biop
compileBiopHead L5.Add = L4.Add
compileBiopHead L5.Sub = L4.Sub
compileBiopHead L5.Mult = L4.Mult
compileBiopHead L5.LessThan = L4.LessThan
compileBiopHead L5.LessThanEqual = L4.LessThanEqual
compileBiopHead L5.Equal = L4.Equal

compilePredHead :: L5.Pred -> L4.Pred
compilePredHead L5.IsNum = L4.IsNum
compilePredHead L5.IsA = L4.IsA


compileX :: L5.X -> L4.X
compileX (L5.Var var) = L4.Var $ compileVariable var

compileVariable :: L5.Variable -> L4.Variable
compileVariable (L5.Variable s) = L4.Variable s

compilePNI :: L5.PosNegInteger -> L4.PosNegInteger
compilePNI (L5.PosNegInteger s) = L4.PosNegInteger s

-- Rename let-bound variables
renamePass :: M.Map L5.X L5.X -> L5.E -> CFS L5.E
renamePass env (L5.Lambda xs e) = liftM (L5.Lambda xs) (renamePass newEnv e)
    where
        newEnv = foldl (\ acc x -> M.delete x acc) env xs

renamePass env e@(L5.Ex x) = case M.lookup x env of
    Nothing -> return e
    Just xN -> return $ L5.Ex xN

renamePass env (L5.Let x d b) = do
    xN <- new5X
    let newEnv = M.insert x xN env
    d_r <- renamePass env d
    b_r <- renamePass newEnv b
    return $ L5.Let xN d_r b_r

renamePass env (L5.LetRec x d b) = do
    xN <- new5X
    let newEnv = M.insert x xN env
    [d_r,b_r] <- mapM (renamePass newEnv) [d,b]
    return $ L5.LetRec xN d_r b_r

renamePass env (L5.If tst thn els) = do
    [tst_r,thn_r,els_r] <- mapM (renamePass env) [tst,thn,els]
    return $ L5.If tst_r thn_r els_r

renamePass env (L5.NewTuple es) = do
    es_r <- mapM (renamePass env) es
    return $ L5.NewTuple es_r

renamePass env (L5.Begin e1 e2) = do
    [e1_r,e2_r] <- mapM (renamePass env) [e1,e2]
    return $ L5.Begin e1_r e2_r

renamePass env (L5.Apply f as) = do
    (f_r:as_r) <- mapM (renamePass env) (f:as)
    return $ L5.Apply f_r as_r

renamePass _ e = return e


