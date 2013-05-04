module L1.Display where

import Data.List (intersperse, sort)

import L1.Grammar


displayProgram :: Program -> String
displayProgram (Program is fs) = "(" ++ displayInstrList is ++ "\n" ++ displayFunctions fs ++ ")"

displayFunctions :: [Function] -> String
displayFunctions = concat . intersperse "\n" . map displayFunction

displayFunction :: Function -> String
displayFunction (Function label is) = "(" ++ displayLabel label ++ "\n" ++ displayInstructions is ++ ")"

displayInstrList :: [Instruction] -> String
displayInstrList is = "(" ++ displayInstructions is ++ ")"

displayInstructions :: [Instruction] -> String
displayInstructions = concat . intersperse "\n" . map displayInstruction

displayInstruction :: Instruction -> String
displayInstruction (Assign r s) = "(" ++ displayReg r ++ " <- " ++ displayS s ++ ")"
displayInstruction (ReadMem r1 r2 n) = "(" ++ displayReg r1 ++ " <- (mem " ++ displayReg r2 ++ " " ++ displayNum n ++ "))"
displayInstruction (Update r n s) = "((mem " ++ displayReg r ++ " " ++ displayNum n ++") <- " ++ displayS s ++ ")"
displayInstruction (Arith r aop t) = "(" ++ displayReg r ++ " " ++ displayAOP aop ++ " " ++ displayT t ++ ")"
displayInstruction (ShiftSX r1 sop r2) = "(" ++ displayReg r1 ++ " " ++ displaySOP sop ++ " " ++ displayReg r2 ++ ")"
displayInstruction (ShiftNum r sop n) = "(" ++ displayReg r ++ " " ++ displaySOP sop ++ " " ++ displayNum n ++ ")"
displayInstruction (SaveCmp r t1 cmp t2) = "(" ++ displayReg r ++ " <- " ++ displayT t1 ++ " " ++ displayCMP cmp ++ " " ++ displayT t2 ++ ")"
displayInstruction (ILab l) = displayLabel l
displayInstruction (Goto l) = "(goto " ++ displayLabel l ++ ")"
displayInstruction (Cjump t1 cmp t2 l1 l2) = "(cjump " ++ displayT t1 ++ " " ++ displayCMP cmp ++ " " ++ displayT t2 ++ " " ++ displayLabel l1 ++ " " ++ displayLabel l2 ++ ")"
displayInstruction (Call u) = "(call " ++ displayU u ++ ")"
displayInstruction (TailCall u) = "(tail-call " ++ displayU u ++ ")"
displayInstruction (Return) = "(return)"
displayInstruction (Print t) = "(eax" ++ " <- (print " ++ displayT t ++ "))"
displayInstruction (Allocate t1 t2) = "(eax" ++ " <- (allocate " ++ displayT t1 ++ " " ++ displayT t2 ++ "))"
displayInstruction (ArrayError t1 t2) = "(eax" ++ " <- (array-error " ++ displayT t1 ++ " " ++ displayT t2 ++ "))" 

displayReg :: Reg -> String
displayReg (ESI) = "esi"
displayReg (EDI) = "edi"
displayReg (EBP) = "ebp"
displayReg (ESP) = "esp"
displayReg (EAX) = "eax"
displayReg (EBX) = "ebx"
displayReg (ECX) = "ecx"
displayReg (EDX) = "edx"

displayS :: S -> String
displayS (Sreg r) = displayReg r
displayS (Snum n) = displayNum n
displayS (Slab l) = displayLabel l

displayT :: T -> String
displayT (Treg r) = displayReg r
displayT (Tnum n) = displayNum n

displayU :: U -> String
displayU (Ureg r) = displayReg r
displayU (Ulab l) = displayLabel l

displayLabel :: Label -> String
displayLabel (Label tok) = ':' : tok

displayNum :: Int -> String
displayNum = show

displayAOP :: AOP -> String
displayAOP (Add) = "+="
displayAOP (Sub) = "-="
displayAOP (Mult) = "*="
displayAOP (And) = "&="

displaySOP :: SOP -> String
displaySOP (ShiftLeft) = "<<="
displaySOP (ShiftRight) = ">>="

displayCMP :: CMP -> String
displayCMP (LessThan) = "<"
displayCMP (LessThanEqual) = "<="
displayCMP (Equal) = "="



