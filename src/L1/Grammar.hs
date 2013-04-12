module L1.Grammar where

data Program = Program [Instruction] [Function]
               deriving (Show)

data Function = Function Label [Instruction]
                deriving (Show)


data Instruction = Assign Reg S
                 | ReadMem Reg Reg Int
                 | Update Reg Int S
                 | Arith Reg AOP T
                 | ShiftSX Reg SOP Reg
                 | ShiftNum Reg SOP Int
                 | SaveCmp Reg T CMP T
                 | ILab Label
                 | Goto Label
                 | Cjump T CMP T Label Label
                 | Call U
                 | Tail_Call U
                 | Return
                 | Print Reg T
                 | Allocate Reg T T
                 | Array_Error Reg T T
                 deriving (Show)


data S = Sreg Reg
       | Snum Int
       | Slab Label
       deriving (Show)

data U = Ureg Reg
       | Ulab Label
       deriving (Show)

data T = Treg Reg
       | Tnum Int
       deriving (Show)

{- Registers -}
data Reg = ESI
         | EDI
         | EBP
         | ESP
         | EDX
         | EBX
         | EAX
         | ECX
         deriving (Show)

{- Operators -}
data AOP = Add
         | Sub
         | Mult
         | And
         deriving (Show)

data SOP = ShiftLeft
         | ShiftRight
         deriving (Show)

data CMP = LessThan
         | LessThanEqual
         | Equal
         deriving (Show)

{- Labels -}
data Label = Label String
             deriving (Show)
