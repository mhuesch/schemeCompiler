module L1Tox86.Compile where


import Control.Monad
import Control.Monad.State
import Control.Monad.Error()

import L1.Grammar

generateAssembly :: Program -> String
generateAssembly p = evalState (assembleProgram p) 0


assembleProgram :: Program -> State Int String
assembleProgram (Program mainBody funs) = do
    mainAssem <- liftM concat $ mapM assembleInstruction mainBody
    funAssem <- liftM concat $ mapM assembleFunction funs
    return $ fileHeader
          ++ mainPrefix
          ++ mainAssem
          ++ mainSuffix
          ++ funAssem
          ++ fileFooter

assembleFunction :: Function -> State Int String
assembleFunction (Function label body) = do
    bodyAssem <- liftM concat $ mapM assembleInstruction body
    return $ funLabel ++ bodyAssem
    where
        funLabel = (colonLabel label) ++ "\n"

assembleInstruction :: Instruction -> State Int String
assembleInstruction (Assign r s) = return $ "movl " ++ assembleS s ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ReadMem r1 r2 n) = return $ "movl " ++ show n ++ "(" ++ assembleReg r2 ++ "), " ++ assembleReg r1 ++ "\n"
assembleInstruction (Update r n s) = return $ "movl " ++ assembleS s ++ ", " ++ show n ++ "(" ++ assembleReg r ++ ")\n"
assembleInstruction (Call u) = do
    count <- get
    let newlabel = "_fun_ret_" ++ show count
    put $ count + 1
    return $ "pushl $" ++ newlabel ++ "\npushl %ebp\nmovl %esp, %ebp\njmp " ++ assembleU u ++ "\n" ++ newlabel ++ ":\n"

-- Arith op
assembleInstruction (Arith r aop t) = return $ aop_c ++ " " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
  where
    aop_c = case aop of
              Add -> "addl"
              Sub -> "subl"
              Mult -> "imull"
              And -> "and"

-- Shift op
assembleInstruction (ShiftSX r1 shift _) = return $ (compileShift shift) ++ " %cl, " ++ assembleReg r1 ++ "\n"
assembleInstruction (ShiftNum r shift n) = return $ (compileShift shift) ++ " " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n"

-- SaveCmp
-- num - num
assembleInstruction (SaveCmp r (Tnum n1) cmp (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 `cmpOp` n2) ++ ", " ++ assembleReg r ++ "\n"
  where
    cmpOp = case cmp of
              LessThan -> (<)
              LessThanEqual -> (<=)
              Equal -> (==)
-- reg - reg
assembleInstruction (SaveCmp r (Treg r2) cmp (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\n" ++ setType ++ " " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setl"
                LessThanEqual -> "setle"
                Equal -> "sete"
-- num - reg
assembleInstruction (SaveCmp r (Tnum n) cmp (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\n" ++ setType ++ " " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setg"
                LessThanEqual -> "setge"
                Equal -> "sete"
-- reg - num
assembleInstruction (SaveCmp r (Treg r2) cmp (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\n" ++ setType ++ " " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setl"
                LessThanEqual -> "setle"
                Equal -> "sete"

-- Cjump
-- reg - reg
assembleInstruction (Cjump (Treg r1) cmp (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\n" ++ jmpType ++ " " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jl"
                LessThanEqual -> "jle"
                Equal -> "je"
-- reg - constant
assembleInstruction (Cjump (Treg r) cmp (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n" ++ jmpType ++ " " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jl"
                LessThanEqual -> "jle"
                Equal -> "je"
-- constant - reg
assembleInstruction (Cjump (Tnum n) cmp (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n" ++ jmpType ++ " " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jg"
                LessThanEqual -> "jge"
                Equal -> "je"
-- constant - constant
assembleInstruction (Cjump (Tnum n1) cmp (Tnum n2) l1 l2) = return $ "jmp " ++ underscoreLabel (if (n1 `cmpOp` n2) then l1 else l2) ++ "\n"
  where
    cmpOp = case cmp of
              LessThan -> (<)
              LessThanEqual -> (<=)
              Equal -> (==)

assembleInstruction (ILab label) = return $ colonLabel label ++ "\n"
assembleInstruction (Goto label) = return $ "jmp " ++ underscoreLabel label ++ "\n"
assembleInstruction (TailCall u) = return $ "movl %ebp, %esp\n    jmp " ++ assembleU u ++ "\n"
assembleInstruction (Return) = return "movl %ebp, %esp\n    pop %ebp\n    ret\n"
assembleInstruction (Print t) = return $ "pushl " ++ assembleT t ++ "\ncall print\naddl $4, %esp\n"
assembleInstruction (Allocate t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall allocate\n" ++ "addl $8,%esp\n"
assembleInstruction (ArrayError t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall print_error\n" ++ "addl $8,%esp\n"

reg8Bit :: Reg -> String
reg8Bit (EAX) = "%al"
reg8Bit (ECX) = "%cl"
reg8Bit (EDX) = "%dl"
reg8Bit (EBX) = "%bl"

assembleCompare :: Bool -> String
assembleCompare test = assembleConstant (if test then 1 else 0)

assembleReg :: Reg -> String
assembleReg (EAX) = "%eax"
assembleReg (ECX) = "%ecx"
assembleReg (ESI) = "%esi"
assembleReg (EDI) = "%edi"
assembleReg (EBP) = "%ebp"
assembleReg (ESP) = "%esp"
assembleReg (EDX) = "%edx"
assembleReg (EBX) = "%ebx"

compileShift :: SOP -> String
compileShift ShiftLeft = "sall"
compileShift ShiftRight = "sarl"

assembleConstant :: Int -> String
assembleConstant n = "$" ++ show n

assembleS :: S -> String
assembleS (Sreg r) = assembleReg r
assembleS (Snum n) = assembleConstant n
assembleS (Slab l) = "$" ++ underscoreLabel l

assembleT :: T -> String
assembleT (Treg r) = assembleReg r
assembleT (Tnum n) = assembleConstant n

assembleU :: U -> String
assembleU (Ureg r) = "*" ++ assembleReg r
assembleU (Ulab l) = underscoreLabel l

colonLabel :: Label -> String
colonLabel (Label name) = "_" ++ name ++ ":"

underscoreLabel :: Label -> String
underscoreLabel (Label name) = "_" ++ name

fileHeader :: String
fileHeader = "    .text\n"
          ++ "    .globl go\n"
          ++ "    .type   go, @function\n"
          ++ "go:\n"

fileFooter :: String
fileFooter = "    .size go, .-go\n"
          ++ "    .section    .note.GNU-stack,\"\",@progbits\n"

mainPrefix :: String
mainPrefix = "pushl   %ebp\n"
          ++ "movl    %esp, %ebp\n"
          ++ "pushl   %ebx\n"
          ++ "pushl   %esi\n"
          ++ "pushl   %edi\n"
          ++ "pushl   %ebp\n"
          ++ "movl    %esp, %ebp\n"

mainSuffix :: String
mainSuffix = "popl   %ebp\n"
          ++ "popl   %edi\n"
          ++ "popl   %esi\n"
          ++ "popl   %ebx\n"
          ++ "leave\n"
          ++ "ret\n"
