module L3ToL2.Compile where


import Control.Monad.State

import qualified L3.AbsL as L3
import qualified L2.AbsL as L2

import Glue



translate :: L3.Program -> L2.Program
translate p = fst $ runCS (compileProgram p) startCS

-- State monad
startCS = CountState 0 0

data CountState = CountState { varCount :: Int
                             , labCount :: Int
                             }

type CS a = State CountState a
runCS = runState


{- Statefully generate new variable -}
newVar = liftM prefixVar getIncVarCount

prefixVar :: Int -> L2.Variable
prefixVar n = L2.Variable $ "v_" ++ show n

getIncVarCount = do
    cs@(CountState vc _) <- get
    put cs{ varCount = vc + 1}
    return vc


{- Statefully generate new label -}
newLab = liftM prefixLab getIncLabCount

prefixLab :: Int -> L2.Label
prefixLab n = L2.Label $ ":l_" ++ show n

getIncLabCount = do
    cs@(CountState _ lc) <- get
    put cs{ labCount = lc + 1 }
    return lc


compileProgram :: L3.Program -> CS L2.Program
compileProgram (L3.Prog body funs) = do
    c_funs <- mapM compileFunction (mainFun:funs)
    return $ L2.Prog (L2.Label mainName) c_funs
    where
        mainName = ":main"
        mainFun = L3.Fun (L3.Label mainName) [] body

compileFunction :: L3.Function -> CS L2.Function
compileFunction (L3.Fun lab vars e) = do
    c_e <- compileE e
    let varAssigns = fetchVars vars
        arityN = intToN2 $ length vars
        spillN = intToN2 0
    return $ L2.Fun (compileLabel lab) arityN spillN (varAssigns ++ c_e)


compileE :: L3.E -> CS [L2.Instruction]
compileE (L3.Let x d e) = do
    c_d <- compileD (compileXW x) False d
    c_e <- compileE e
    return $ c_d ++ c_e

compileE (L3.If v e1 e2) = do
    e1_label <- newLab
    e2_label <- newLab
    c_e1 <- compileE e1
    c_e2 <- compileE e2
    return . concat $ [[L2.ICjump (encodeT v) L2.Equal (L2.Tnum $ intToN2 1) e2_label e1_label]
                      ,[L2.ILabel e1_label]
                      ,c_e1
                      ,[L2.ILabel e2_label]
                      ,c_e2]

compileE (L3.Ed d) = compileD L2.RAX True d

variableToW :: L2.Variable -> L2.W
variableToW = L2.Wcx . L2.Var

compileD :: L2.W -> Bool -> L3.D -> CS [L2.Instruction]
compileD target isTail (L3.Binop biop v1 v2) = do
    tmp <- newVar
    let tmpW = variableToW tmp
    liftM (addReturn isTail) $ case biop of
        L3.Add -> return [L2.IAssign tmpW (encodeS v1)
                        ,L2.IArith tmpW L2.Add (encodeT v2)
                        ,L2.IArith tmpW L2.Sub (L2.Tnum $ intToN2 1)
                        ,L2.IAssign target (L2.Sx $ L2.Xw tmpW)]
        L3.Sub -> return [L2.IAssign tmpW (encodeS v1)
                        ,L2.IArith tmpW L2.Sub (encodeT v2)
                        ,L2.IArith tmpW L2.Add (L2.Tnum $ intToN2 1)
                        ,L2.IAssign target (L2.Sx $ L2.Xw tmpW)]
        L3.Mult -> do
            return $ [L2.IAssign tmpW (encodeS v1)
                     ,L2.IShiftN tmpW L2.ShiftRight (intToN2 1)
                     ,L2.IAssign target (encodeS v2)
                     ,L2.IShiftN target L2.ShiftRight (intToN2 1)
                     ,L2.IArith target L2.Mult (L2.Tx (L2.Xw tmpW))]
                     ++ encodeInstrs target
        _ -> let cmp = compileCMP biop
             in return $ [L2.ISaveCmp target (encodeT v1) cmp (encodeT v2)]
                         ++ encodeInstrs target

compileD target isTail (L3.Predicate pr v) = return . addReturn isTail $
    [L2.IAssign target (encodeS v)
    ,L2.IArith target L2.And (L2.Tnum $ intToN2 1)]
    ++ case pr of
            L3.IsNum -> encodeInstrs target
            L3.IsA -> [L2.IArith target L2.Mult (L2.Tnum $ intToN2 (-2))
                     ,L2.IArith target L2.Add (L2.Tnum $ intToN2 3)]

-- Must set return label
compileD target isTail (L3.Apply v vs) = do
    retLab <- newLab
    let callArityPNI = intToPNI2 $ length vs
    case isTail of
      False -> return . (addReturn isTail) $ (setReturnLabel retLab):(setVars vs)
                                             ++ [L2.ICallNative (encodeU v) callArityPNI
                                                ,L2.ILabel retLab
                                                ,L2.IAssign target raxS]
      True -> return $ setVars vs
                       ++ [L2.ITailCall (encodeU v) callArityPNI]

compileD target isTail (L3.NewArray v1 v2) = return . addReturn isTail $
    (setVars [v1,v2]) ++
    [L2.ICallRuntime L2.Allocate (intToPNI2 2)
    ,L2.IAssign target raxS]

compileD target isTail (L3.NewTuple vs) = do
    tmp <- newVar
    let tmpW = variableToW tmp
        offsets = map (L2.Num . L2.PosNegInteger . show) [8,16..]
        vSs = map encodeS vs
        body = (setVars [L3.Vnum . intToPNI3 . length $ vs, L3.Vnum $ intToPNI3 1])
               ++ [L2.ICallRuntime L2.Allocate (intToPNI2 2)
                  ,L2.IAssign tmpW raxS]
               ++ zipWith (L2.IWriteMem (L2.Xw tmpW)) offsets vSs
               ++ [L2.IAssign target raxS]
    return . addReturn isTail $ body

compileD target isTail (L3.Aref v1 v2) = do
    tmp1 <- newVar
    tmp2 <- newVar
    notBadLabel <- newLab
    badLabel <- newLab
    goodLabel <- newLab
    let v1X =  compileVX v1
        tmp1W = variableToW tmp1
        tmp2W = variableToW tmp2
    return . addReturn isTail $
        [L2.IAssign tmp1W (encodeS v2)
        ,L2.IShiftN tmp1W L2.ShiftRight (intToN2 1)
        ,L2.IReadMem tmp2W v1X (intToN2 0)
        ,L2.ICjump (L2.Tx $ L2.Xw tmp1W) L2.LessThan (L2.Tx $ L2.Xw tmp2W) notBadLabel badLabel
        ,L2.ILabel notBadLabel
        ,L2.ICjump (L2.Tx $ L2.Xw tmp1W) L2.LessThan (L2.Tnum $ intToN2 0) badLabel goodLabel
        ,L2.ILabel badLabel]
        ++
        (setVars [v1,v2])
        ++
        [L2.ICallRuntime L2.ArrayError (intToPNI2 2)
        ,L2.ILabel goodLabel
        ,L2.IArith tmp1W L2.Mult (L2.Tnum $ intToN2 8)
        ,L2.IArith tmp1W L2.Add (L2.Tx v1X)
        ,L2.IReadMem target (L2.Xw tmp1W) (intToN2 8)]

compileD target isTail (L3.Aset v1 v2 v3) = do
    tmp1 <- newVar
    tmp2 <- newVar
    notBadLabel <- newLab
    badLabel <- newLab
    goodLabel <- newLab
    let v1X =  compileVX v1
        tmp1W = variableToW tmp1
        tmp2W = variableToW tmp2
    return . addReturn isTail $
        [L2.IAssign tmp1W (encodeS v2)
        ,L2.IShiftN tmp1W L2.ShiftRight (intToN2 1)
        ,L2.IReadMem tmp2W v1X (intToN2 0)
        ,L2.ICjump (L2.Tx $ L2.Xw tmp1W) L2.LessThan (L2.Tx $ L2.Xw tmp2W) notBadLabel badLabel
        ,L2.ILabel notBadLabel
        ,L2.ICjump (L2.Tx $ L2.Xw tmp1W) L2.LessThan (L2.Tnum $ intToN2 0) badLabel goodLabel
        ,L2.ILabel badLabel]
        ++
        (setVars [v1,v2])
        ++
        [L2.ICallRuntime L2.ArrayError (intToPNI2 2)
        ,L2.ILabel goodLabel
        ,L2.IArith tmp1W L2.Mult (L2.Tnum $ intToN2 8)
        ,L2.IArith tmp1W L2.Add (L2.Tx v1X)
        ,L2.IWriteMem (L2.Xw tmp1W) (intToN2 8) (encodeS v3)
        ,L2.IAssign target (L2.Snum $ intToN2 1)]

compileD target isTail (L3.Alen v) = return . addReturn isTail $
    [L2.IReadMem target (compileVX v) (intToN2 0)]
    ++ encodeInstrs target

compileD target isTail (L3.Print v) = return . addReturn isTail $
    (setVars [v])
    ++
    [L2.ICallRuntime L2.Print (intToPNI2 1)
    ,L2.IAssign target raxS]

compileD target isTail (L3.MakeClosure proc vars) = compileD target isTail (L3.NewTuple [L3.Vlab proc,vars])

compileD target isTail (L3.ClosureProc clos) = compileD target isTail (L3.Aref clos (L3.Vnum $ intToPNI3 0))

compileD target isTail (L3.ClosureVars clos) = compileD target isTail (L3.Aref clos (L3.Vnum $ intToPNI3 1))

compileD target isTail (L3.Dv v) = return . addReturn isTail $
    [L2.IAssign target (encodeS v)]


addReturn :: Bool -> [L2.Instruction] -> [L2.Instruction]
addReturn True = (++ [L2.IReturn])
addReturn False = id

raxS = (L2.Sx raxX)
raxX = L2.Xw L2.RAX


compileCMP :: L3.Biop -> L2.CMP
compileCMP (L3.LessThan) = L2.LessThan
compileCMP (L3.LessThanEqual) = L2.LessThanEqual
compileCMP (L3.Equal) = L2.Equal

encodeInstrs :: L2.W -> [L2.Instruction]
encodeInstrs w = [L2.IArith w L2.Mult (L2.Tnum $ intToN2 2)
                 ,L2.IArith w L2.Add (L2.Tnum $ intToN2 1)]

compileXX :: L3.X -> L2.X
compileXX = L2.Xw . compileXW

compileXW :: L3.X -> L2.W
compileXW = L2.Wcx . L2.Var . compileXVar

compileVX :: L3.V -> L2.X
compileVX (L3.Vx x) = compileXX x

encodeU :: L3.V -> L2.U
encodeU (L3.Vx x) = L2.Ux $ compileXX x
encodeU (L3.Vlab l) = L2.Ulab $ compileLabel l

encodeS :: L3.V -> L2.S
encodeS (L3.Vx x) = L2.Sx $ compileXX x
encodeS (L3.Vnum (L3.PosNegInteger n)) = L2.Snum . intToN2 $ (2*(read n) + 1)
encodeS (L3.Vlab l) = L2.Slab $ compileLabel l

encodeT :: L3.V -> L2.T
encodeT (L3.Vx x) = L2.Tx $ compileXX x
encodeT (L3.Vnum (L3.PosNegInteger n)) = L2.Tnum . intToN2 $ (2*(read n) + 1)

setReturnLabel :: L2.Label -> L2.Instruction
setReturnLabel l = L2.IWriteMem L2.RSP (L2.Num . L2.PosNegInteger . show $ -8) (L2.Slab l)

setVars :: [L3.V] -> [L2.Instruction]
setVars vs = setRegArgs ++ setStackArgs
    where
        regNum   = length argRegW
        forRegs  = map encodeS . take regNum $ vs
        forStack = map encodeS . drop regNum $ vs
        setRegArgs   = zipWith L2.IAssign argRegW forRegs
        setStackArgs = zipWith (L2.IWriteMem L2.RSP) offsetNs forStack
        --
        stackCount = length forStack
        offsetNs = map (L2.Num . L2.PosNegInteger . show) $ [-16,-24..]

fetchVars :: [L3.X] -> [L2.Instruction]
fetchVars vs = fetchRegArgs ++ fetchStackArgs
    where
        regNum = length argRegW
        vWs = map (L2.Wcx . L2.Var . compileXVar) vs
        fromRegs  = take regNum vWs
        fromStack = drop regNum vWs
        fetchRegArgs   = zipWith L2.IAssign fromRegs (map (L2.Sx . L2.Xw) argRegW)
        fetchStackArgs = zipWith L2.IReadArg fromStack offsetNs
        --
        stackCount = length fromStack
        offsetNs = map (L2.Num . L2.PosNegInteger . show) . reverse . take stackCount $ [0,8..]


compileXVar :: L3.X -> L2.Variable
compileXVar (L3.Var (L3.Variable name)) = L2.Variable name

compileLabel :: L3.Label -> L2.Label
compileLabel (L3.Label name) = L2.Label name


intToPNI2 :: Int -> L2.PosNegInteger
intToPNI2 = L2.PosNegInteger . show

intToPNI3 :: Int -> L3.PosNegInteger
intToPNI3 = L3.PosNegInteger . show

intToN2 :: Int -> L2.N
intToN2 = L2.Num . intToPNI2
