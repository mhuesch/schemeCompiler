module L2ToL1.Compile where


import Data.Maybe
import Control.Monad.Reader

import Glue
import L2.Grammar
import L1.Grammar
import Spill.Spill
import Liveness.Liveness
import Graph.GraphGlue
import Graph.Interference
import Graph.Color


compileProgram :: L2Program -> Program
compileProgram (L2Program m fs) = (Program (compileMain m) (map compileFun fs))


compileMain :: [L2Instruction] -> [Instruction]
compileMain ls = case stackSpillOffset of
    0 -> newLs
    _ -> concat [[decESP],newLs,[incESP]]
    where
        decESP = Arith ESP Sub (Tnum stackSpillOffset)
        incESP = Arith ESP Add (Tnum stackSpillOffset)
        stackSpillOffset = numSpills * 4
        (newLs,numSpills) = compileInstructionsCountSpills 0 ls

compileFun :: L2Function -> Function
compileFun (L2Function label body) = (Function (compileLabel label) (compileInstructions body))

compileInstructions :: [L2Instruction] -> [Instruction]
compileInstructions ls = case stackSpillOffset of
    0 -> newLs
    _ -> decESP:newLs
    where
        decESP = Arith ESP Sub (Tnum stackSpillOffset)
        stackSpillOffset = numSpills * 4
        (newLs,numSpills) = compileInstructionsCountSpills 0 ls


compileInstructionsCountSpills :: Int -> [L2Instruction] -> ([Instruction],Int)
compileInstructionsCountSpills numSpills ls = case runColor iG of
    (Just cs) -> (colorizeInstructions cs ls, numSpills)
    Nothing -> let varToSpill = head vars
                   stackOffset = (-4) * (numSpills + 1)
                   spillPrefix = makeSpillPrefix numSpills
               in compileInstructionsCountSpills (numSpills + 1) $ spill ls varToSpill stackOffset spillPrefix
    where
        lR@(LivenessResult _ vars) = liveRes ls
        iG = buildInterference lR


makeSpillPrefix :: Int -> L2Var
makeSpillPrefix n = L2Var $ "s_" ++ show n ++ "_"


colorizeInstructions :: Coloring -> [L2Instruction] -> [Instruction]
colorizeInstructions cs ls = runReader (mapM colorInstruction ls) cs


type RC = Reader Coloring

colorInstruction :: L2Instruction -> RC Instruction
colorInstruction (L2Assign x s) = do
    c_x <- compileX x
    c_s <- compileS s
    return $ Assign c_x c_s

colorInstruction (L2ReadMem x1 x2 n) = do
    c_x1 <- compileX x1
    c_x2 <- compileX x2
    return $ ReadMem c_x1 c_x2 n

colorInstruction (L2Update x n s) = do
    c_x <- compileX x
    c_s <- compileS s
    return $ Update c_x n c_s

colorInstruction (L2Arith x aop t) = do
    c_x <- compileX x
    c_t <- compileT t
    return $ Arith c_x (compileAOP aop) c_t

colorInstruction (L2ShiftSX x1 sop x2) = do
    c_x1 <- compileX x1
    c_x2 <- compileX x2
    return $ ShiftSX c_x1 (compileSOP sop) c_x2

colorInstruction (L2ShiftNum x sop n) = do
    c_x <- compileX x
    return $ ShiftNum c_x (compileSOP sop) n

colorInstruction (L2SaveCmp x t1 cmp t2) = do
    c_x <- compileX x
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return $ SaveCmp c_x c_t1 (compileCMP cmp) c_t2

colorInstruction (L2ILab l) = return . ILab $ compileLabel l

colorInstruction (L2Goto l)  = return . Goto $ compileLabel l

colorInstruction (L2Cjump t1 cmp t2 l1 l2) = do
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return $ Cjump c_t1 (compileCMP cmp) c_t2 (compileLabel l1) (compileLabel l2)

colorInstruction (L2Call u) = do
    c_u <- compileU u
    return $ Call c_u

colorInstruction (L2TailCall u) = do
    c_u <- compileU u
    return $ TailCall c_u

colorInstruction (L2Return) = do
    return $ Return

colorInstruction (L2Print t) = do
    c_t <- compileT t
    return $ Print c_t

colorInstruction (L2Allocate t1 t2) = do
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return $ Allocate c_t1 c_t2

colorInstruction (L2ArrayError t1 t2) = do
    c_t1 <- compileT t1
    c_t2 <- compileT t2
    return $ ArrayError c_t1 c_t2



compileS :: L2S -> RC S
compileS (L2SX x) = liftM Sreg $ compileX x
compileS (L2Snum n) = return (Snum n)
compileS (L2Slab l) = return . Slab $ compileLabel l

compileU :: L2U -> RC U
compileU (L2UX x) = liftM Ureg $ compileX x
compileU (L2Ulab l) = return . Ulab . compileLabel $ l

compileT :: L2T -> RC T
compileT (L2TX x) = liftM Treg $ compileX x
compileT (L2Tnum n) = return $ Tnum n

compileX :: L2X -> RC Reg
compileX (L2Xreg r) = return $ compileReg r
compileX (L2Xvar v) = compileVar v

compileVar :: L2Var -> RC Reg
compileVar v = do
    cs <- ask
    case lookupColor cs (L2Xvar v) of
        Nothing -> error $ "var " ++ show v ++ " wasn't colored"
        Just (r) -> return $ compileReg r

compileAOP :: L2AOP -> AOP
compileAOP L2Add = Add
compileAOP L2Sub = Sub
compileAOP L2Mult = Mult
compileAOP L2And = And

compileSOP :: L2SOP -> SOP
compileSOP L2ShiftLeft = ShiftLeft
compileSOP L2ShiftRight = ShiftRight

compileCMP :: L2CMP -> CMP
compileCMP L2LessThan = LessThan
compileCMP L2LessThanEqual = LessThanEqual
compileCMP L2Equal = Equal

compileReg :: L2Reg -> Reg
compileReg L2ESI = ESI
compileReg L2EDI = EDI
compileReg L2EBP = EBP
compileReg L2ESP = ESP
compileReg L2EDX = EDX
compileReg L2EBX = EBX
compileReg L2EAX = EAX
compileReg L2ECX = ECX

compileLabel :: L2Label -> Label
compileLabel (L2Label l) = (Label l)
