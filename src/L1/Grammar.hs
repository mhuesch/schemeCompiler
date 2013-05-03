module L1.Grammar where

import Control.Applicative
import Test.QuickCheck

data Program = Program [Instruction] [Function]
               deriving (Show,Eq)

data Function = Function Label [Instruction]
                deriving (Show,Eq)


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
                 | TailCall U
                 | Return
                 | Print T
                 | Allocate T T
                 | ArrayError T T
                 deriving (Show,Eq)


data S = Sreg Reg
       | Snum Int
       | Slab Label
       deriving (Show,Eq)

data U = Ureg Reg
       | Ulab Label
       deriving (Show,Eq)

data T = Treg Reg
       | Tnum Int
       deriving (Show,Eq)

{- Registers -}
data Reg = ESI
         | EDI
         | EBP
         | ESP
         | EDX
         | EBX
         | EAX
         | ECX
         deriving (Show,Eq)

{- Operators -}
data AOP = Add
         | Sub
         | Mult
         | And
         deriving (Show,Eq)

data SOP = ShiftLeft
         | ShiftRight
         deriving (Show,Eq)

data CMP = LessThan
         | LessThanEqual
         | Equal
         deriving (Show,Eq)

{- Labels -}
data Label = Label String
             deriving (Show,Eq)


{- Arbitrary instances -}
instance Arbitrary Program where
    arbitrary = Program <$> arbitrary <*> arbitrary

instance Arbitrary Function where
    arbitrary = Function <$> arbitrary <*> arbitrary

instance Arbitrary Instruction where
    arbitrary = oneof [Assign <$> arbitrary <*> arbitrary
                      ,ReadMem <$> arbitrary <*> arbitrary <*> arbitrary
                      ,Update <$> arbitrary <*> arbitrary <*> arbitrary
                      ,Arith <$> arbitrary <*> arbitrary <*> arbitrary
                      ,ShiftSX <$> arbitrary <*> arbitrary <*> arbitrary
                      ,ShiftNum <$> arbitrary <*> arbitrary <*> arbitrary
                      ,SaveCmp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ,ILab <$> arbitrary
                      ,Goto <$> arbitrary
                      ,Cjump <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ,Call <$> arbitrary
                      ,TailCall <$> arbitrary
                      ,return Return
                      ,Print <$> arbitrary
                      ,Allocate <$> arbitrary <*> arbitrary
                      ,ArrayError <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary S where
    arbitrary = oneof [Sreg <$> arbitrary
                      ,Slab <$> arbitrary
                      ,Snum <$> arbitrary
                      ]

instance Arbitrary U where
    arbitrary = oneof [Ureg <$> arbitrary
                      ,Ulab <$> arbitrary
                      ]

instance Arbitrary T where
    arbitrary = oneof [Treg <$> arbitrary
                      ,Tnum <$> arbitrary
                      ]

instance Arbitrary Reg where
    arbitrary = elements [ESI
                         ,EDI
                         ,EBP
                         ,ESP
                         ,EDX
                         ,EBX
                         ,EAX
                         ,ECX
                         ]

instance Arbitrary AOP where
    arbitrary = elements [Add
                         ,Sub
                         ,Mult
                         ,And
                         ]

instance Arbitrary SOP where
    arbitrary = elements [ShiftLeft
                         ,ShiftRight
                         ]

instance Arbitrary CMP where
    arbitrary = elements [LessThan
                         ,LessThanEqual
                         ,Equal
                         ]

instance Arbitrary Label where
    arbitrary = do
        v <- elements (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
        vs <- listOf $ elements (['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        return $ Label (v:vs)




