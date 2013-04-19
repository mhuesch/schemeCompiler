module L2.Display where

import Data.List (intersperse, sort)

import L2.Grammar

{- Liveness display variable list -}
displayXList :: [L2X] -> String
displayXList xs = "(" ++ (concat . intersperse " " . sort $ map displayX xs) ++ ")"
{--------}

displayProgram :: L2Program -> String
displayProgram (L2Program is fs) = "(" ++ displayInstrList is ++ displayFunctions fs ++ ")"

displayFunctions :: [L2Function] -> String
displayFunctions = concat . intersperse "\n" . map displayFunction

displayFunction :: L2Function -> String
displayFunction (L2Function label is) = "(" ++ displayLabel label ++ displayInstructions is ++ ")"

displayInstrList :: [L2Instruction] -> String
displayInstrList is = "(" ++ displayInstructions is ++ ")"

displayInstructions :: [L2Instruction] -> String
displayInstructions = concat . intersperse "\n" . map displayInstruction

displayInstruction :: L2Instruction -> String
displayInstruction (L2Assign x s) = "(" ++ displayX x ++ " <- " ++ displayS s ++ ")"
displayInstruction (L2ReadMem x1 x2 n) = "(" ++ displayX x1 ++ " <- (mem " ++ displayX x2 ++ " " ++ displayNum n ++ "))"
displayInstruction (L2Update x n s) = "((mem " ++ displayX x ++ " " ++ displayNum n ++") <- " ++ displayS s ++ ")"
displayInstruction (L2Arith x aop t) = "(" ++ displayX x ++ " " ++ displayAOP aop ++ " " ++ displayT t ++ ")"
displayInstruction (L2ShiftSX x1 sop x2) = "(" ++ displayX x1 ++ " " ++ displaySOP sop ++ " " ++ displayX x2 ++ ")"
displayInstruction (L2ShiftNum x sop n) = "(" ++ displayX x ++ " " ++ displaySOP sop ++ " " ++ displayNum n ++ ")"
displayInstruction (L2SaveCmp x t1 cmp t2) = "(" ++ displayX x ++ " <- " ++ displayT t1 ++ " " ++ displayCMP cmp ++ " " ++ displayT t2 ++ ")"
displayInstruction (L2ILab l) = displayLabel l
displayInstruction (L2Goto l) = "(goto " ++ displayLabel l ++ ")"
displayInstruction (L2Cjump t1 cmp t2 l1 l2) = "(cjump " ++ displayT t1 ++ " " ++ displayCMP cmp ++ " " ++ displayT t2 ++ " " ++ displayLabel l1 ++ " " ++ displayLabel l2 ++ ")"
displayInstruction (L2Call u) = "(call " ++ displayU u ++ ")"
displayInstruction (L2TailCall u) = "(tail-call " ++ displayU u ++ ")"
displayInstruction (L2Return) = "(return)"
displayInstruction (L2Print t) = "(eax" ++ " <- (print " ++ displayT t ++ "))"
displayInstruction (L2Allocate t1 t2) = "(eax" ++ " <- (allocate " ++ displayT t1 ++ " " ++ displayT t2 ++ "))"
displayInstruction (L2ArrayError t1 t2) = "(eax" ++ " <- (array-error " ++ displayT t1 ++ " " ++ displayT t2 ++ "))"

displayX :: L2X -> String
displayX (L2Xreg r) = displayReg r
displayX (L2Xvar v) = displayVar v

displayReg :: L2Reg -> String
displayReg (L2ESI) = "esi"
displayReg (L2EDI) = "edi"
displayReg (L2EBP) = "ebp"
displayReg (L2ESP) = "esp"
displayReg (L2EAX) = "eax"
displayReg (L2EBX) = "ebx"
displayReg (L2ECX) = "ecx"
displayReg (L2EDX) = "edx"

displayVar :: L2Var -> String
displayVar (L2Var s) = s

displayS :: L2S -> String
displayS (L2SX x) = displayX x
displayS (L2Snum n) = displayNum n
displayS (L2Slab l) = displayLabel l

displayLabel :: L2Label -> String
displayLabel (L2Label tok) = ':' : tok

displayNum :: Int -> String
displayNum = show

displayAOP :: L2AOP -> String
displayAOP (L2Add) = "+="
displayAOP (L2Sub) = "-="
displayAOP (L2Mult) = "*="
displayAOP (L2And) = "&="

displaySOP :: L2SOP -> String
displaySOP (L2ShiftLeft) = "<<="
displaySOP (L2ShiftRight) = ">>="

displayCMP :: L2CMP -> String
displayCMP (L2LessThan) = "<"
displayCMP (L2LessThanEqual) = "<="
displayCMP (L2Equal) = "="

displayT :: L2T -> String
displayT (L2TX x) = displayX x
displayT (L2Tnum n) = displayNum n

displayU :: L2U -> String
displayU (L2UX x) = displayX x
displayU (L2Ulab l) = displayLabel l



