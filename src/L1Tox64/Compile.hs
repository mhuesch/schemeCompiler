module L1Tox64.Compile where


import Control.Monad
import Control.Monad.State
import Control.Monad.Error()

import L1_64.AbsL1

generateAssembly :: Program -> String
generateAssembly p = evalState (assembleProgram p) 0


assembleProgram :: Program -> State Int String
assembleProgram (Prog mainBody funs) = do
    mainAssem <- liftM concat $ mapM assembleInstruction mainBody
    funAssem <- liftM concat $ mapM assembleFunction funs
    return $ fileHeader
          ++ mainPrefix
          ++ mainAssem
          ++ mainSuffix
          ++ funAssem
          ++ fileFooter

assembleFunction :: Function -> State Int String
assembleFunction (Fun label body) = do
    bodyAssem <- liftM concat $ mapM assembleInstruction body
    return $ funLabel ++ bodyAssem
    where
        funLabel = (standaloneLabel label) ++ "\n"

assembleInstruction :: Instruction -> State Int String
assembleInstruction (IAssign x s) = return $ "movq " ++ assembleS s ++ ", " ++ assembleX x ++ "\n"
assembleInstruction (IReadMem x1 x2 n) = return $ "movq " ++ showN n ++ "(" ++ assembleX x2 ++ "), " ++ assembleX x1 ++ "\n"
assembleInstruction (IWriteMem x n s) = return $ "movq " ++ assembleS s ++ ", " ++ showN n ++ "(" ++ assembleX x ++ ")\n"
assembleInstruction (ICall u) = do
    count <- get
    let newlabel = "_fun_ret_" ++ show count
    put $ count + 1
    return $ "pushq $" ++ newlabel ++ "\npushq " ++ assembleX RBP ++ "\nmovq " ++ assembleX RSP ++ ", " ++ assembleX RBP ++ "\njmp " ++ assembleU u ++ "\n" ++ newlabel ++ ":\n"

-- Arith op
assembleInstruction (IArith x aop t) = return $ aop_c ++ " " ++ assembleT t ++ ", " ++ assembleX x ++ "\n"
  where
    aop_c = case aop of
              Add -> "addq"
              Sub -> "subq"
              Mult -> "imulq"
              And -> "andq"

-- Shift op
assembleInstruction (IShiftCX x1 shift cx) = return $ (compileShift shift) ++ " " ++ xLowByte (Xcx cx) ++ ", " ++ assembleX x1 ++ "\n"
assembleInstruction (IShiftN x shift n) = return $ (compileShift shift) ++ " " ++ assembleN n ++ ", " ++ assembleX x ++ "\n"

-- SaveCmp
-- num - num
assembleInstruction (ISaveCmp x (Tnum n1) cmp (Tnum n2)) = return $ "movq " ++ assembleCompare (n1 `cmpOp` n2) ++ ", " ++ assembleX x ++ "\n"
  where
    cmpOp = case cmp of
              LessThan -> (<)
              LessThanEqual -> (<=)
              Equal -> (==)
-- reg - reg
assembleInstruction (ISaveCmp x (Tx x2) cmp (Tx x3)) = return $ "cmpq " ++ assembleX x3 ++ ", " ++ assembleX x2 ++ "\n" ++ setType ++ " " ++ xLowByte x ++ "\nmovzbq " ++ xLowByte x ++ ", " ++ assembleX x ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setl"
                LessThanEqual -> "setle"
                Equal -> "sete"
-- num - reg
assembleInstruction (ISaveCmp x (Tnum n) cmp (Tx x2)) = return $ "cmpq " ++ assembleN n ++ ", " ++ assembleX x2 ++ "\n" ++ setType ++ " " ++ xLowByte x ++ "\nmovzbq " ++ xLowByte x ++ ", " ++ assembleX x ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setg"
                LessThanEqual -> "setge"
                Equal -> "sete"
-- reg - num
assembleInstruction (ISaveCmp x (Tx x2) cmp (Tnum n)) = return $ "cmpq " ++ assembleN n ++ ", " ++ assembleX x2 ++ "\n" ++ setType ++ " " ++ xLowByte x ++ "\nmovzbq " ++ xLowByte x ++ ", " ++ assembleX x ++ "\n"
  where
    setType = case cmp of
                LessThan -> "setl"
                LessThanEqual -> "setle"
                Equal -> "sete"

-- Cjump
-- reg - reg
assembleInstruction (ICjump (Tx x1) cmp (Tx x2) l1 l2) = return $ "cmpq " ++ assembleX x2 ++ ", " ++ assembleX x1 ++ "\n" ++ jmpType ++ " " ++ inlineLabel l1 ++ "\njmp " ++ inlineLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jl"
                LessThanEqual -> "jle"
                Equal -> "je"
-- reg - constant
assembleInstruction (ICjump (Tx x) cmp (Tnum n) l1 l2) = return $ "cmpq " ++ assembleN n ++ ", " ++ assembleX x ++ "\n" ++ jmpType ++ " " ++ inlineLabel l1 ++ "\njmp " ++ inlineLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jl"
                LessThanEqual -> "jle"
                Equal -> "je"
-- constant - reg
assembleInstruction (ICjump (Tnum n) cmp (Tx x) l1 l2) = return $ "cmpq " ++ assembleN n ++ ", " ++ assembleX x ++ "\n" ++ jmpType ++ " " ++ inlineLabel l1 ++ "\njmp " ++ inlineLabel l2 ++ "\n"
  where
    jmpType = case cmp of
                LessThan -> "jg"
                LessThanEqual -> "jge"
                Equal -> "je"
-- constant - constant
assembleInstruction (ICjump (Tnum n1) cmp (Tnum n2) l1 l2) = return $ "jmp " ++ inlineLabel (if (n1 `cmpOp` n2) then l1 else l2) ++ "\n"
  where
    cmpOp = case cmp of
              LessThan -> (<)
              LessThanEqual -> (<=)
              Equal -> (==)

assembleInstruction (ILabel label) = return $ standaloneLabel label ++ "\n"
assembleInstruction (IGoto label) = return $ "jmp " ++ inlineLabel label ++ "\n"
assembleInstruction (ITailCall u) = return $ "movq " ++ assembleX RBP ++ ", " ++ assembleX RSP ++ "\n    jmp " ++ assembleU u ++ "\n"
assembleInstruction (IReturn) = return $ "movq " ++ assembleX RBP ++ ", " ++ assembleX RSP ++ "\n    pop " ++ assembleX RBP ++ "\n    ret\n"
assembleInstruction (IPrint _ t) = return $ "movq " ++ assembleT t ++ ", " ++ assembleX RDI ++ "\ncall print\n"
assembleInstruction (IAllocate _ t1 t2) = return $ "movq " ++ assembleT t1 ++ ", " ++ assembleX RDI ++ "\nmovq " ++ assembleT t2 ++ ", " ++ assembleX RSI ++ "\ncall allocate\n"
assembleInstruction (IArrayError _ t1 t2) = return $ "movq " ++ assembleT t1 ++ ", " ++ assembleX RDI ++ "\nmovq " ++ assembleT t2 ++ ", " ++ assembleX RSI ++ "\ncall print_error\n"

xLowByte :: X -> String
xLowByte (Xax RAX) = "%al"
xLowByte (Xcx RCX) = "%cl"
xLowByte (RDX)     = "%dl"
xLowByte (RBX)     = "%bl"
xLowByte (RSI)     = "%sil"
xLowByte (RDI)     = "%dil"
xLowByte (RBP)     = "%bpl"
xLowByte (RSP)     = "%spl"
xLowByte (R8)      = "%r8b"
xLowByte (R9)      = "%r9b"
xLowByte (R10)     = "%r10b"
xLowByte (R11)     = "%r11b"
xLowByte (R12)     = "%r12b"
xLowByte (R13)     = "%r13b"
xLowByte (R14)     = "%r14b"
xLowByte (R15)     = "%r15b"

assembleCompare :: Bool -> String
assembleCompare test = assembleN . Num $ (if test then 1 else 0)

assembleX :: X -> String
assembleX (Xax RAX) = "%rax"
assembleX (Xcx RCX) = "%rcx"
assembleX (RBX)     = "%rbx"
assembleX (RDX)     = "%rdx"
assembleX (RSI)     = "%rsi"
assembleX (RDI)     = "%rdi"
assembleX (RBP)     = "%rbp"
assembleX (RSP)     = "%rsp"
assembleX (R8)      = "%r8"
assembleX (R9)      = "%r9"
assembleX (R10)     = "%r10"
assembleX (R11)     = "%r11"
assembleX (R12)     = "%r12"
assembleX (R13)     = "%r13"
assembleX (R14)     = "%r14"
assembleX (R15)     = "%r15"

compileShift :: SOP -> String
compileShift ShiftLeft = "salq"
compileShift ShiftRight = "sarq"

assembleN :: N -> String
assembleN (Num int) = "$" ++ show int

showN :: N -> String
showN (Num int) = show int

assembleS :: S -> String
assembleS (Sx x) = assembleX x
assembleS (Snum n) = assembleN n
assembleS (Slab l) = "$" ++ inlineLabel l

assembleT :: T -> String
assembleT (Tx x) = assembleX x
assembleT (Tnum n) = assembleN n

assembleU :: U -> String
assembleU (Ux x) = "*" ++ assembleX x
assembleU (Ulab l) = inlineLabel l

-- Leading colon must be dropped from label
standaloneLabel :: Label -> String
standaloneLabel (Label name) = "_" ++ tail name ++ ":"

inlineLabel :: Label -> String
inlineLabel (Label name) = "_" ++ tail name

fileHeader :: String
fileHeader = "    .text\n"
          ++ "    .globl go\n"
          ++ "    .type   go, @function\n"
          ++ "go:\n"

fileFooter :: String
fileFooter = "    .size go, .-go\n"
          ++ "    .section    .note.GNU-stack,\"\",@progbits\n"

mainPrefix :: String
mainPrefix = "pushq   %rbp\n"
          ++ "movq    %rsp, %rbp\n"
          ++ "pushq   %rbx\n"
          ++ "pushq   %rsi\n"
          ++ "pushq   %rdi\n"
          ++ "pushq   %rbp\n"
          ++ "movq    %rsp, %rbp\n"

mainSuffix :: String
mainSuffix = "popq   %rbp\n"
          ++ "popq   %rdi\n"
          ++ "popq   %rsi\n"
          ++ "popq   %rbx\n"
          ++ "leave\n"
          ++ "ret\n"
