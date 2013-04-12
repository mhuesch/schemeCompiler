module L2.Grammar where

data L2Program = L2Program [L2Instruction] [L2Function]
                 deriving (Show, Eq)

data L2Function = L2Function L2Label [L2Instruction]
                deriving (Show, Eq)


data L2Instruction = L2Assign L2X L2S
                   | L2ReadMem L2X L2X Int
                   | L2Update L2X Int L2S
                   | L2Arith L2X L2AOP L2T
                   | L2ShiftSX L2X L2SOP L2X
                   | L2ShiftNum L2X L2SOP Int
                   | L2SaveCmp L2X L2T L2CMP L2T
                   | L2ILab L2Label
                   | L2Goto L2Label
                   | L2Cjump L2T L2CMP L2T L2Label L2Label
                   | L2Call L2U
                   | L2Tail_Call L2U
                   | L2Return
                   | L2Print L2X L2T
                   | L2Allocate L2X L2T L2T
                   | L2Array_Error L2X L2T L2T
                   deriving (Show, Eq)


data L2S = L2SX L2X
         | L2Snum Int
         | L2Slab L2Label
         deriving (Show, Eq)

data L2U = L2UX L2X
         | L2Ulab L2Label
         deriving (Show, Eq)

data L2T = L2TX L2X
         | L2Tnum Int
         deriving (Show, Eq)

data L2X = L2Xreg L2Reg
         | L2Xvar L2Var
         deriving (Show, Eq)

{- Registers -}
data L2Reg = L2ESI
           | L2EDI
           | L2EBP
           | L2ESP
           | L2EDX
           | L2EBX
           | L2EAX
           | L2ECX
         deriving (Show, Eq)

{- Variables -}
data L2Var = L2Var String
           deriving (Show, Eq)

{- Operators -}
data L2AOP = L2Add
           | L2Sub
           | L2Mult
           | L2And
           deriving (Show, Eq)

data L2SOP = L2ShiftLeft
           | L2ShiftRight
           deriving (Show, Eq)

data L2CMP = L2LessThan
           | L2LessThanEqual
           | L2Equal
           deriving (Show, Eq)

{- Labels -}
data L2Label = L2Label String
             deriving (Show, Eq)
