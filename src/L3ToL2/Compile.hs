module L3ToL2.Compile where


import Control.Monad.State

import L3.Grammar
import L2.Grammar



translate :: L3Program -> L2Program
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

prefixVar :: Int -> L2Var
prefixVar n = L2Var $ "v_" ++ show n

getIncVarCount = do
    cs@(CountState vc _) <- get
    put cs{ varCount = vc + 1}
    return vc


{- Statefully generate new label -}
newLab = liftM prefixLab getIncLabCount

prefixLab :: Int -> L2Label
prefixLab n = L2Label $ "l_" ++ show n

getIncLabCount = do
    cs@(CountState _ lc) <- get
    put cs{ labCount = lc + 1 }
    return lc


compileProgram :: L3Program -> CS L2Program
compileProgram (L3Program body funs) = do
    c_funs <- mapM compileFunction (mainFun:funs)
    return $ L2Program [callMain] c_funs
    where
        mainName = "main"
        callMain = L2Call . L2Ulab . L2Label $ mainName
        mainFun = L3Function (L3Label mainName) [] body

compileFunction :: L3Function -> CS L2Function
compileFunction (L3Function lab vars e) = do
    let varAssigns = fetchVars vars
    c_e <- compileE e
    return $ L2Function (compileLabel lab) (varAssigns ++ c_e)


compileE :: L3E -> CS [L2Instruction]
compileE (L3Let x d e) = do
    c_d <- compileD (L2Xvar $ compileXVar x) False d
    c_e <- compileE e
    return $ c_d ++ c_e

compileE (L3If v e1 e2) = do
    e1_label <- newLab
    e2_label <- newLab
    c_e1 <- compileE e1
    c_e2 <- compileE e2
    return . concat $ [[L2Cjump (encodeT v) L2Equal (L2Tnum 1) e2_label e1_label]
                      ,[L2ILab e1_label]
                      ,c_e1
                      ,[L2ILab e2_label]
                      ,c_e2]

compileE (L3Ed d) = compileD (eaxX) True d


compileD :: L2X -> Bool -> L3D -> CS [L2Instruction]
compileD target isTail (L3Binop biop v1 v2) = liftM (addReturn isTail) $ case biop of
    L3Add -> return [L2Assign target (encodeS v1)
                    ,L2Arith target L2Add (encodeT v2)
                    ,L2Arith target L2Sub (L2Tnum 1)]
    L3Sub -> return [L2Assign target (encodeS v1)
                    ,L2Arith target L2Sub (encodeT v2)
                    ,L2Arith target L2Add (L2Tnum 1)]
    L3Mult -> do
        tmp <- liftM L2Xvar newVar
        return $ [L2Assign tmp (encodeS v1)
                 ,L2ShiftNum tmp L2ShiftRight 1
                 ,L2Assign target (encodeS v2)
                 ,L2ShiftNum target L2ShiftRight 1
                 ,L2Arith target L2Mult (L2TX tmp)]
                 ++ encodeInstrs target
    _ -> let cmp = compileCMP biop
         in return $ [L2SaveCmp target (encodeT v1) cmp (encodeT v2)]
                     ++ encodeInstrs target

compileD target isTail (L3Predicate pr v) = return . addReturn isTail $
    [L2Assign target (encodeS v)
    ,L2Arith target L2And (L2Tnum 1)]
    ++ case pr of
            L3IsNumber -> encodeInstrs target
            L3IsA -> [L2Arith target L2Mult (L2Tnum (-2))
                     ,L2Arith target L2Add (L2Tnum 3)]

compileD target isTail (L3Apply v vs) = case isTail of
    False -> return . (addReturn isTail) $ setVars vs
                                           ++ [L2Call (encodeU v)
                                              ,L2Assign target eaxS]
    True -> return $ setVars vs
                     ++ [L2TailCall (encodeU v)]

compileD target isTail (L3NewArray v1 v2) = return . addReturn isTail $
    [L2Allocate (encodeT v1) (encodeT v2)
    ,L2Assign target eaxS]

compileD target isTail (L3NewTuple vs) = do
    let offsets = [4,8..]
        tupleSs = map encodeS vs
        body = [L2Allocate (L2Tnum (2*(length vs) + 1)) (L2Tnum 1)
               ,L2Assign target eaxS]
               ++ zipWith (\ offset s -> L2Update target offset s) offsets tupleSs
    return . addReturn isTail $ body

compileD target isTail (L3Aref v1 v2) = do
    tmp <- newVar
    notBadLabel <- newLab
    badLabel <- newLab
    goodLabel <- newLab
    let v1X =  compileVX v1
        tmpX = L2Xvar tmp
    return . addReturn isTail $
        [L2Assign target (encodeS v2)
        ,L2ShiftNum target L2ShiftRight 1
        ,L2ReadMem tmpX v1X 0
        ,L2Cjump (L2TX target) L2LessThan (L2TX tmpX) notBadLabel badLabel
        ,L2ILab notBadLabel
        ,L2Cjump (L2TX target) L2LessThan (L2Tnum 0) badLabel goodLabel
        ,L2ILab badLabel
        ,L2ArrayError (L2TX v1X) (encodeT v2)
        ,L2ILab goodLabel
        ,L2Arith target L2Mult (L2Tnum 4)
        ,L2Arith target L2Add (L2TX v1X)
        ,L2ReadMem target target 4]

compileD target isTail (L3Aset v1 v2 v3) = do
    tmp <- newVar
    notBadLabel <- newLab
    badLabel <- newLab
    goodLabel <- newLab
    let v1X =  compileVX v1
        tmpX = L2Xvar tmp
    return . addReturn isTail $
        [L2Assign target (encodeS v2)
        ,L2ShiftNum target L2ShiftRight 1
        ,L2ReadMem tmpX v1X 0
        ,L2Cjump (L2TX target) L2LessThan (L2TX tmpX) notBadLabel badLabel
        ,L2ILab notBadLabel
        ,L2Cjump (L2TX target) L2LessThan (L2Tnum 0) badLabel goodLabel
        ,L2ILab badLabel
        ,L2ArrayError (L2TX v1X) (encodeT v2)
        ,L2ILab goodLabel
        ,L2Arith target L2Mult (L2Tnum 4)
        ,L2Arith target L2Add (L2TX v1X)
        ,L2Update target 4 (encodeS v3)
        ,L2Assign target (L2Snum 1)]

compileD target isTail (L3Alen v) = return . addReturn isTail $
    [L2ReadMem target (compileVX v) 0]
    ++ encodeInstrs target

compileD target isTail (L3Print v) = return . addReturn isTail $
    [L2Print (encodeT v)
    ,L2Assign target eaxS]

compileD target isTail (L3MakeClosure proc vars) = compileD target isTail (L3NewTuple [L3Vlab proc,vars])

compileD target isTail (L3ClosureProc clos) = compileD target isTail (L3Aref clos (L3Vnum 0))

compileD target isTail (L3ClosureVars clos) = compileD target isTail (L3Aref clos (L3Vnum 1))

compileD target isTail (L3Dv v) = return . addReturn isTail $
    [L2Assign target (encodeS v)]


addReturn :: Bool -> [L2Instruction] -> [L2Instruction]
addReturn True = (++ [L2Return])
addReturn False = id

eaxS = (L2SX eaxX)
eaxX = L2Xreg L2EAX


compileCMP :: L3biop -> L2CMP
compileCMP (L3LessThan) = L2LessThan
compileCMP (L3LessThanEqual) = L2LessThanEqual
compileCMP (L3Equal) = L2Equal

encodeInstrs :: L2X -> [L2Instruction]
encodeInstrs x = [L2Arith x L2Mult (L2Tnum 2)
                 ,L2Arith x L2Add (L2Tnum 1)]

compileXX :: L3X -> L2X
compileXX = L2Xvar . compileXVar

compileVX :: L3V -> L2X
compileVX (L3Vx x) = compileXX x

encodeU :: L3V -> L2U
encodeU (L3Vx x) = L2UX $ compileXX x
encodeU (L3Vlab l) = L2Ulab $ compileLabel l

encodeS :: L3V -> L2S
encodeS (L3Vx x) = L2SX $ compileXX x
encodeS (L3Vnum n) = L2Snum (2*n + 1)
encodeS (L3Vlab l) = L2Slab $ compileLabel l

encodeT :: L3V -> L2T
encodeT (L3Vx x) = L2TX $ compileXX x
encodeT (L3Vnum n) = L2Tnum (2*n + 1)

setVars :: [L3V] -> [L2Instruction]
setVars vars = zipWith (\ x s -> L2Assign x s) regXs xSs
    where
        xSs = map encodeS vars
        regXs = map (L2Xreg) funArgRegs ++ [undefined]

fetchVars :: [L3X] -> [L2Instruction]
fetchVars vars = zipWith (\ x s -> L2Assign x s) xs regSs
    where
        xs = map (L2Xvar . compileXVar) vars
        regSs = map (L2SX . L2Xreg) funArgRegs ++ [undefined]

funArgRegs :: [L2Reg]
funArgRegs = [L2ECX, L2EDX, L2EAX]

compileXVar :: L3X -> L2Var
compileXVar (L3X name) = L2Var name

compileLabel :: L3Label -> L2Label
compileLabel (L3Label name) = L2Label name






