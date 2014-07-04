module L1Tox64.Compile where


import Control.Monad
import Control.Monad.Reader
import Control.Monad.Identity
import Data.List

import L1.AbsL1


assembleProgram :: Program -> String
assembleProgram (Prog mainLabel funs) = fileHeader
                                        ++ mainPrefix
                                        ++ "movq $ret_main, -8(%rsp)\n"
                                        ++ ("jmp " ++ inlineLabel mainLabel ++ "\n")
                                        ++ "ret_main:\n"
                                        ++ mainSuffix
                                        ++ funAssem
                                        ++ fileFooter
  where
    funAssem = concatMap assembleFunction funs

data FunctionInfo = FunctionInfo { arity :: Integer
                                 , spills :: Integer }

myFrameOffset :: FunctionInfo -> Integer
myFrameOffset fi = 8 * count
  where
    ar = arity fi
    sp = spills fi
    count = 1 + sp + max 0 (ar - 6)

myArgsSpillsOffset :: FunctionInfo -> Integer
myArgsSpillsOffset fi = 8 * count
  where
    ar = arity fi
    sp = spills fi
    count = sp + max 0 (ar - 6)

assembleFunction :: Function -> String
assembleFunction (Fun label arity' spills' body) = intercalate "\n" [funLabel, rspAdjust, bodyAssem]
    where
        bodyAssem = runIdentity $ runReaderT (liftM concat $ mapM assembleInstruction body) fi
        funLabel = standaloneLabel label
        fi = FunctionInfo (nToInteger arity') (nToInteger spills')
        rspAdjust = "subq " ++ assembleConstant (myFrameOffset fi) ++ ", %rsp"


assembleInstruction :: Instruction -> ReaderT FunctionInfo Identity String
assembleInstruction (IAssign w s) = return $ "movq " ++ assembleS s ++ ", " ++ assembleX (Xw w) ++ "\n"

assembleInstruction (IReadMem w x n) = return $ "movq " ++ showN n ++ "(" ++ assembleX x ++ "), " ++ assembleX (Xw w) ++ "\n"

assembleInstruction (IWriteMem x n s) = return $ "movq " ++ assembleS s ++ ", " ++ showN n ++ "(" ++ assembleX x ++ ")\n"

-- Arith op
assembleInstruction (IArith w aop t) = return $ aop_c ++ " " ++ assembleT t ++ ", " ++ assembleX (Xw w) ++ "\n"
  where
    aop_c = case aop of
              Add -> "addq"
              Sub -> "subq"
              Mult -> "imulq"
              And -> "andq"

-- Shift op
assembleInstruction (IShiftCX w shift cx) = return $ (compileShift shift) ++ " " ++ xLowByte (Xw (Wcx cx)) ++ ", " ++ assembleX (Xw w) ++ "\n"
assembleInstruction (IShiftN w shift n) = return $ (compileShift shift) ++ " " ++ assembleN n ++ ", " ++ assembleX (Xw w) ++ "\n"

-- SaveCmp
-- num - num
assembleInstruction (ISaveCmp w (Tnum n1) cmp (Tnum n2)) = return $ "movq " ++ assembleCompare (n1 `cmpOp` n2) ++ ", " ++ assembleX (Xw w) ++ "\n"
  where
    cmpOp = compareN $ case cmp of
                         LessThan -> (<)
                         LessThanEqual -> (<=)
                         Equal -> (==)
-- reg - reg
assembleInstruction (ISaveCmp w (Tx x2) cmp (Tx x3)) = return $ intercalate "\n" ["cmpq " ++ assembleX x3 ++ ", " ++ assembleX x2
                                                                                 ,setType ++ " " ++ xLowByte (Xw w)
                                                                                 ,"movzbq " ++ xLowByte (Xw w) ++ ", " ++ assembleX (Xw w) ++ "\n"]
  where
    setType = case cmp of
                LessThan -> "setl"
                LessThanEqual -> "setle"
                Equal -> "sete"
-- num - reg
assembleInstruction (ISaveCmp w (Tnum n) cmp (Tx x2)) = return $ intercalate "\n" ["cmpq " ++ assembleN n ++ ", " ++ assembleX x2
                                                                                  ,setType ++ " " ++ xLowByte (Xw w)
                                                                                  ,"movzbq " ++ xLowByte (Xw w) ++ ", " ++ assembleX (Xw w) ++ "\n"]
  where
    setType = case cmp of
                LessThan -> "setg"
                LessThanEqual -> "setge"
                Equal -> "sete"
-- reg - num
assembleInstruction (ISaveCmp w (Tx x2) cmp (Tnum n)) = return $ intercalate "\n" ["cmpq " ++ assembleN n ++ ", " ++ assembleX x2
                                                                                  ,setType ++ " " ++ xLowByte (Xw w)
                                                                                  ,"movzbq " ++ xLowByte (Xw w) ++ ", " ++ assembleX (Xw w) ++ "\n"]
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
    cmpOp = compareN $ case cmp of
                         LessThan -> (<)
                         LessThanEqual -> (<=)
                         Equal -> (==)

assembleInstruction (ILabel label) = return $ standaloneLabel label ++ "\n"

assembleInstruction (IGoto label) = return $ "jmp " ++ inlineLabel label ++ "\n"

assembleInstruction (ICallNative u _) = return $ "jmp " ++ assembleU u ++ "\n"

assembleInstruction (ICallRuntime r _) = return $ "call " ++ rName ++ "\n"
  where
    rName = case r of
              Print ->      "print"
              Allocate ->   "allocate"
              ArrayError -> "print_error"

assembleInstruction (ITailCall u _) = do
    offset <- asks myFrameOffset
    return $ intercalate "\n" ["addq " ++ assembleConstant offset ++ ", %rsp"
                              ,"jmp " ++ assembleU u ++ "\n"]

assembleInstruction (IReturn) = do
    offset <- asks myArgsSpillsOffset
    return $ intercalate "\n" ["addq " ++ assembleConstant offset ++ ", %rsp"
                              ,"ret\n"]


xLowByte :: X -> String
xLowByte RSP = "%spl"
xLowByte (Xw (Wcx RCX)) = "%cl"
xLowByte (Xw RAX) = "%al"
xLowByte (Xw RBX) = "%dl"
xLowByte (Xw RDX) = "%bl"
xLowByte (Xw RSI) = "%sil"
xLowByte (Xw RDI) = "%dil"
xLowByte (Xw RBP) = "%bpl"
xLowByte (Xw R8)  = "%r8b"
xLowByte (Xw R9)  = "%r9b"
xLowByte (Xw R10) = "%r10b"
xLowByte (Xw R11) = "%r11b"
xLowByte (Xw R12) = "%r12b"
xLowByte (Xw R13) = "%r13b"
xLowByte (Xw R14) = "%r14b"
xLowByte (Xw R15) = "%r15b"

assembleCompare :: Bool -> String
assembleCompare test = assembleN . Num . PosNegInteger $ (if test then "1" else "0")

assembleX :: X -> String
assembleX RSP = "%rsp"
assembleX (Xw (Wcx RCX)) = "%rcx"
assembleX (Xw RAX) = "%rax"
assembleX (Xw RBX) = "%rbx"
assembleX (Xw RDX) = "%rdx"
assembleX (Xw RSI) = "%rsi"
assembleX (Xw RDI) = "%rdi"
assembleX (Xw RBP) = "%rbp"
assembleX (Xw R8)  = "%r8"
assembleX (Xw R9)  = "%r9"
assembleX (Xw R10) = "%r10"
assembleX (Xw R11) = "%r11"
assembleX (Xw R12) = "%r12"
assembleX (Xw R13) = "%r13"
assembleX (Xw R14) = "%r14"
assembleX (Xw R15) = "%r15"

compileShift :: SOP -> String
compileShift ShiftLeft = "salq"
compileShift ShiftRight = "sarq"

assembleConstant :: Integer -> String
assembleConstant i = "$" ++ show i

assembleN :: N -> String
assembleN (Num (PosNegInteger i)) = "$" ++ i

showN :: N -> String
showN (Num (PosNegInteger i)) = i

compareN :: (Integer -> Integer -> Bool) -> N -> N -> Bool
compareN cmp n1 n2 = nToInteger n1 `cmp` nToInteger n2

nToInteger :: N -> Integer
nToInteger (Num (PosNegInteger i)) = (read i :: Integer)

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

fileFooter :: String
fileFooter = "    .size go, .-go\n"
          ++ "    .section    .note.GNU-stack,\"\",@progbits\n"

