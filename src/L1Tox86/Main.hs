{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import System.Environment
import System.IO
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)
import Text.ParserCombinators.Parsec hiding (State)

import L1.Grammar
import L1.Parser

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    assembleFile (args !! 0)


assembleFile :: FilePath -> IO ()
assembleFile fp = do
    runtimeOExists <- doesFileExist "runtime.o"
    when (not runtimeOExists) $ putStrLn "No runtime.o. Exiting."
    contents <- readFile fp
    let filename = "prog.S"
        (prog, _) = runState (generateAssembly contents) 0
    writeFile filename prog
    rawSystem "as" ["--32", "-o", "prog.o", "prog.S"]
    rawSystem "gcc" ["-m32", "-o", "a.out", "prog.o", "runtime.o"]
    return ()



generateAssembly :: MonadState Int m => String -> m String
generateAssembly = assembleProgram . readProg

assembleProgram :: MonadState Int m => Program -> m String
assembleProgram (Program mainBody funs) = do
    mainAssem <- liftM concat $ mapM assembleInstruction mainBody
    funAssem <- liftM concat $ mapM assembleFunction funs
    return $ fileHeader
          ++ mainPrefix
          ++ mainAssem
          ++ mainSuffix
          ++ funAssem
          ++ fileFooter

assembleFunction :: MonadState Int m => Function -> m String
assembleFunction (Function label body) = do
    bodyAssem <- liftM concat $ mapM assembleInstruction body
    return $ funLabel ++ bodyAssem
    where
        funLabel = (colonLabel label) ++ "\n"

assembleInstruction :: MonadState Int m => Instruction -> m String
assembleInstruction (Assign r s) = return $ "movl " ++ assembleS s ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ReadMem r1 r2 n) = return $ "movl " ++ show n ++ "(" ++ assembleReg r2 ++ "), " ++ assembleReg r1 ++ "\n"
assembleInstruction (Update r n s) = return $ "movl " ++ assembleS s ++ ", " ++ show n ++ "(" ++ assembleReg r ++ ")\n"
assembleInstruction (Call u) = do
    count <- get
    let newlabel = "_fun_ret_" ++ show count
    put $ count + 1
    return $ "pushl $" ++ newlabel ++ "\npushl %ebp\nmovl %esp, %ebp\njmp " ++ assembleU u ++ "\n" ++ newlabel ++ ":\n"


-- Arith op
assembleInstruction (Arith r Add t) = return $ "addl " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r Sub t) = return $ "subl " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r Mult t) = return $ "imull " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (Arith r And t) = return $ "and " ++ assembleT t ++ ", " ++ assembleReg r ++ "\n"

-- Shift op
assembleInstruction (ShiftSX r1 ShiftLeft r2) = return $ "sall %cl, " ++ assembleReg r1 ++ "\n"
assembleInstruction (ShiftSX r1 ShiftRight r2) = return $ "sarl %cl, " ++ assembleReg r1 ++ "\n"
assembleInstruction (ShiftNum r ShiftLeft n) = return $ "sall " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (ShiftNum r ShiftRight n) = return $ "sarl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\n"

-- SaveCmp
-- num - num
assembleInstruction (SaveCmp r (Tnum n1) LessThan (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 < n2) ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n1) LessThanEqual (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 <= n2) ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n1) Equal (Tnum n2)) = return $ "movl " ++ assembleCompare (n1 == n2) ++ ", " ++ assembleReg r ++ "\n"
-- reg - reg
assembleInstruction (SaveCmp r (Treg r2) LessThan (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsetl " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) LessThanEqual (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsetle " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) Equal (Treg r3)) = return $ "cmpl " ++ assembleReg r3 ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
-- num - reg
assembleInstruction (SaveCmp r (Tnum n) LessThan (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetg " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n) LessThanEqual (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetge " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Tnum n) Equal (Treg r2)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
-- reg - num
assembleInstruction (SaveCmp r (Treg r2) LessThan (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetl " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) LessThanEqual (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsetle " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"
assembleInstruction (SaveCmp r (Treg r2) Equal (Tnum n)) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r2 ++ "\nsete " ++ reg8Bit r ++ "\nmovzbl " ++ reg8Bit r ++ ", " ++ assembleReg r ++ "\n"

-- Cjump
-- reg - reg
assembleInstruction (Cjump (Treg r1) LessThan (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\njl " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r1) LessThanEqual (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\njle " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r1) Equal (Treg r2) l1 l2) = return $ "cmpl " ++ assembleReg r2 ++ ", " ++ assembleReg r1 ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- reg - constant
assembleInstruction (Cjump (Treg r) LessThan (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njl " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r) LessThanEqual (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njle " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Treg r) Equal (Tnum n) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- constant - reg
assembleInstruction (Cjump (Tnum n) LessThan (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njg " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Tnum n) LessThanEqual (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\njge " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
assembleInstruction (Cjump (Tnum n) Equal (Treg r) l1 l2) = return $ "cmpl " ++ assembleConstant n ++ ", " ++ assembleReg r ++ "\nje " ++ underscoreLabel l1 ++ "\njmp " ++ underscoreLabel l2 ++ "\n"
-- constant - constant
assembleInstruction (Cjump (Tnum n1) LessThan (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 < n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"
assembleInstruction (Cjump (Tnum n1) LessThanEqual (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 <= n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"
assembleInstruction (Cjump (Tnum n1) Equal (Tnum n2) l1 l2) = return $ "jmp " ++ (if (n1 == n2) then underscoreLabel l1 else underscoreLabel l2) ++ "\n"

assembleInstruction (ILab label) = return $ colonLabel label ++ "\n"
assembleInstruction (Goto label) = return $ "jmp " ++ underscoreLabel label ++ "\n"
assembleInstruction (Tail_Call u) = return $ "movl %ebp, %esp\n    jmp " ++ assembleU u ++ "\n"
assembleInstruction (Return) = return $ "movl %ebp, %esp\n    pop %ebp\n    ret\n"
assembleInstruction (Print r t) = return $ "pushl " ++ assembleT t ++ "\ncall print\naddl $4, %esp\n"
assembleInstruction (Allocate r t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall allocate\n" ++ "addl $8,%esp\n"
assembleInstruction (Array_Error r t1 t2) = return $ "pushl " ++ assembleT t2 ++ "\npushl " ++ assembleT t1 ++ "\ncall print_error\n" ++ "addl $8,%esp\n"

reg8Bit :: Reg -> String
reg8Bit (EAX) = "%al"
reg8Bit (ECX) = "%cl"
reg8Bit (EDX) = "%dl"
reg8Bit (EBX) = "%bl"

assembleCompare :: Bool -> String
assembleCompare test = if test
                          then assembleConstant 1
                          else assembleConstant 0

assembleReg :: Reg -> String
assembleReg (EAX) = "%eax"
assembleReg (ECX) = "%ecx"
assembleReg (ESI) = "%esi"
assembleReg (EDI) = "%edi"
assembleReg (EBP) = "%ebp"
assembleReg (ESP) = "%esp"
assembleReg (EDX) = "%edx"
assembleReg (EBX) = "%ebx"

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


{- L1Error -}
data L1Error = Parser ParseError
             | Default String

instance Show L1Error where
    show = showError

instance Error L1Error where
    noMsg = Default "An error has occurred"
    strMsg = Default

showError :: L1Error -> String
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either L1Error

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
{- End L1Error -}







