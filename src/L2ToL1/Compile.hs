module L2ToL1.Compile where


import Data.List (isPrefixOf)
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

import Glue
import qualified L2.AbsL2 as L2
import L2.PrintL2
import L1.AbsL1
import Spill.Spill
import Liveness.Liveness
import Graph.Interference
import Graph.Color


intBitWidth :: Int
intBitWidth = 8

type RE a = ReaderT String (ErrorT String Identity) a
runRSE msg ev = runIdentity (runErrorT (runReaderT ev msg))


translate :: L2.Program -> Either String Program
translate p = runRSE "could not register allocate" (compileProgram p)


compileProgram :: L2.Program -> RE Program
compileProgram (L2.Prog l fs) = do
    let c_l = compileLabel l
    c_fs <- mapM compileFun fs
    return $ Prog c_l c_fs

data InstrOrStackArg = Inst Instruction
                     | SA   W N

compileFun :: L2.Function -> RE Function
compileFun (L2.Fun label@(L2.Label name) arity spills body) = do
    (instrs,spillCount) <- local (++ " function " ++ name) $ compileInstructionsCountSpills (nToInt spills) (tempCalleeSave body)
    let fixedInstrs = converStackReads spillCount instrs
        spillN = Num . PosNegInteger . show $ spillCount
    return $ Fun (compileLabel label) (compileN arity) spillN fixedInstrs

converStackReads :: Int -> [InstrOrStackArg] -> [Instruction]
converStackReads spills = map f
  where
    f (Inst i) = i
    f (SA w (Num (PosNegInteger i))) = let offset = read i + (intBitWidth * spills)
                                           newN = Num . PosNegInteger . show $ offset
                                         in IReadMem w RSP newN


tempCalleeSave :: [L2.Instruction] -> [L2.Instruction]
tempCalleeSave ls = toTemp ++ (concatMap f ls)
    where
        f i@L2.IReturn{} = fromTemp ++ [i]
        f i@L2.ITailCall{} = fromTemp ++ [i]
        f i = [i]
        --
        toTemp = map (\ w -> L2.IAssign (makeTempW w) (L2.Sx (L2.Xw w))) calleeSaveRegW
        fromTemp = map (\ w -> L2.IAssign w (L2.Sx . L2.Xw $ makeTempW w)) calleeSaveRegW
        --
        makeTempW w = L2.Wcx . L2.Var . L2.Variable $ printTree w ++ "_temp"


compileInstructionsCountSpills :: Int -> [L2.Instruction] -> RE ([InstrOrStackArg],Int)
compileInstructionsCountSpills numSpills ls = do
    let lR@(LivenessResult _ vars) = liveRes ls
        iG = buildInterference lR
    case runColor iG of
        (Just cs) -> return (colorizeInstructions cs ls, numSpills)
        Nothing -> case getNonSpilledVars vars of
            [] -> do
                msg <- ask
                throwError msg
            (v:_) -> let varToSpill = v
                         stackOffset = intBitWidth * numSpills
                         prefix = makeSpillPrefix numSpills
                         spilledBody = spill ls varToSpill stackOffset prefix
                     in compileInstructionsCountSpills (numSpills + 1) spilledBody



spillPrefix = "s_"

getNonSpilledVars :: [L2.Variable] -> [L2.Variable]
getNonSpilledVars = filter (\ (L2.Variable name) -> not $ isPrefixOf spillPrefix name)

makeSpillPrefix :: Int -> L2.Variable
makeSpillPrefix n = L2.Variable $ spillPrefix ++ show n ++ "_"


colorizeInstructions :: Coloring -> [L2.Instruction] -> [InstrOrStackArg]
colorizeInstructions cs ls = runReader (mapM colorInstruction ls) cs


type RC = Reader Coloring

colorInstruction :: L2.Instruction -> RC InstrOrStackArg
colorInstruction (L2.IAssign w s) = do
    c_w <- compileW w
    c_s <- compileS s
    return . Inst $ IAssign c_w c_s

colorInstruction (L2.IReadMem w x n) = do
    c_w <- compileW w
    c_x <- compileX x
    return . Inst $ IReadMem c_w c_x (compileN n)

colorInstruction (L2.IReadArg w n) = do
    c_w <- compileW w
    return $ SA c_w (compileN n)

colorInstruction (L2.IWriteMem x n s) = do
    c_x <- compileX x
    c_s <- compileS s
    return . Inst $ IWriteMem c_x (compileN n) c_s

colorInstruction (L2.IArith w aop t) = do
    c_w <- compileW w
    c_t <- compileT t
    return . Inst $ IArith c_w (compileAOP aop) c_t

colorInstruction (L2.IShiftCX w sop cx) = do
    c_w <- compileW w
    c_cx <- compileCX cx
    return . Inst $ IShiftCX c_w (compileSOP sop) c_cx

colorInstruction (L2.IShiftN w sop n) = do
    c_w <- compileW w
    return . Inst $ IShiftN c_w (compileSOP sop) (compileN n)

colorInstruction (L2.ISaveCmp w t1 cmp t2) = do
    c_w <- compileW w
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return . Inst $ ISaveCmp c_w c_t1 (compileCMP cmp) c_t2

colorInstruction (L2.ILabel l) = return . Inst . ILabel $ compileLabel l

colorInstruction (L2.IGoto l)  = return . Inst . IGoto $ compileLabel l

colorInstruction (L2.ICjump t1 cmp t2 l1 l2) = do
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return . Inst $ ICjump c_t1 (compileCMP cmp) c_t2 (compileLabel l1) (compileLabel l2)

colorInstruction (L2.ICallNative u pni) = do
    c_u <- compileU u
    let c_pni = compilePNI pni
    return . Inst $ ICallNative c_u c_pni

colorInstruction (L2.ICallRuntime r pni) = do
    let c_r = compileR r
        c_pni = compilePNI pni
    return . Inst $ ICallRuntime c_r c_pni

colorInstruction (L2.ITailCall u pni) = do
    c_u <- compileU u
    return . Inst $ ITailCall c_u (compilePNI pni)

colorInstruction (L2.IReturn) = do
    return . Inst $ IReturn


compileS :: L2.S -> RC S
compileS (L2.Sx x) = liftM Sx $ compileX x
compileS (L2.Snum n) = return . Snum . compileN $ n
compileS (L2.Slab l) = return . Slab . compileLabel $ l

compileU :: L2.U -> RC U
compileU (L2.Ux x) = liftM Ux $ compileX x
compileU (L2.Ulab l) = return . Ulab . compileLabel $ l

compileT :: L2.T -> RC T
compileT (L2.Tx x) = liftM Tx $ compileX x
compileT (L2.Tnum n) = return . Tnum  . compileN $ n

compileX :: L2.X -> RC X
compileX L2.RSP = return RSP
compileX (L2.Xw w) = liftM Xw $ compileW w

compileW :: L2.W -> RC W
compileW L2.RAX = return RAX
compileW L2.RBX = return RBX
compileW L2.RDX = return RDX
compileW L2.RDI = return RDI
compileW L2.RSI = return RSI
compileW L2.RBP = return RBP
compileW L2.R8  = return R8
compileW L2.R9  = return R9
compileW L2.R10 = return R10
compileW L2.R11 = return R11
compileW L2.R12 = return R12
compileW L2.R13 = return R13
compileW L2.R14 = return R14
compileW L2.R15 = return R15
compileW (L2.Wcx L2.RCX) = return (Wcx RCX)
compileW (L2.Wcx v) = do
    cs <- ask
    case lookupColor cs (L2.Wcx v) of
      Just (L2.Wcx (L2.Var var)) -> err var
      Just colored_w -> compileW colored_w
      _ -> err v
  where
    err v = error $ "var " ++ show v ++ " wasn't colored"

compileCX :: L2.CX -> RC CX
compileCX L2.RCX = return RCX
compileCX cx@(L2.Var v) = do
    cs <- ask
    case lookupColor cs (L2.Wcx cx) of
      Just (L2.Wcx L2.RCX) -> return RCX
      _ -> error $ "var" ++ show v ++ " wasn't colored"

{-
compileVar :: L2.Var -> RC Reg
compileVar v = do
    cs <- ask
    case lookupColor cs (L2.Xvar v) of
        Nothing -> error $ "var " ++ show v ++ " wasn't colored"
        Just (r) -> return $ compileReg r
-}

compileAOP :: L2.AOP -> AOP
compileAOP L2.Add = Add
compileAOP L2.Sub = Sub
compileAOP L2.Mult = Mult
compileAOP L2.And = And

compileSOP :: L2.SOP -> SOP
compileSOP L2.ShiftLeft = ShiftLeft
compileSOP L2.ShiftRight = ShiftRight

compileCMP :: L2.CMP -> CMP
compileCMP L2.LessThan = LessThan
compileCMP L2.LessThanEqual = LessThanEqual
compileCMP L2.Equal = Equal

compileR :: L2.R -> R
compileR L2.Print = Print
compileR L2.Allocate = Allocate
compileR L2.ArrayError = ArrayError

compileLabel :: L2.Label -> Label
compileLabel (L2.Label l) = (Label l)

compilePNI :: L2.PosNegInteger -> PosNegInteger
compilePNI (L2.PosNegInteger i) = PosNegInteger i

compileN :: L2.N -> N
compileN (L2.Num pni) = Num $ compilePNI pni


-- 
-- Misc helpers

nToInt :: L2.N -> Int
nToInt (L2.Num (L2.PosNegInteger i)) = read i

