module L2.Grammar where

import Control.Applicative
import Test.QuickCheck

data L2Spill = L2Spill [L2Instruction] L2Var Int L2Var
               deriving (Show)

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
                   | L2TailCall L2U
                   | L2Return
                   | L2Print L2X L2T
                   | L2Allocate L2X L2T L2T
                   | L2ArrayError L2X L2T L2T
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


{- Arbitrary instances -}
instance Arbitrary L2Program where
    arbitrary = L2Program <$> arbitrary <*> arbitrary

instance Arbitrary L2Function where
    arbitrary = L2Function <$> arbitrary <*> arbitrary

instance Arbitrary L2Instruction where
    arbitrary = oneof [L2Assign <$> arbitrary <*> arbitrary
                      ,L2ReadMem <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2Update <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2Arith <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2ShiftSX <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2ShiftNum <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2SaveCmp <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ,L2ILab <$> arbitrary
                      ,L2Goto <$> arbitrary
                      ,L2Cjump <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      ,L2Call <$> arbitrary
                      ,L2TailCall <$> arbitrary
                      ,return L2Return
                      ,L2Print <$> arbitrary <*> arbitrary
                      ,L2Allocate <$> arbitrary <*> arbitrary <*> arbitrary
                      ,L2ArrayError <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary L2S where
    arbitrary = oneof [L2SX <$> arbitrary
                      ,L2Slab <$> arbitrary
                      ,L2Snum <$> arbitrary
                      ]

instance Arbitrary L2U where
    arbitrary = oneof [L2Ulab <$> arbitrary
                      ,L2UX <$> arbitrary
                      ]

instance Arbitrary L2T where
    arbitrary = oneof [L2Tnum <$> arbitrary
                      ,L2TX <$> arbitrary
                      ]

instance Arbitrary L2X where
    arbitrary = oneof [L2Xreg <$> arbitrary
                      ,L2Xvar <$> arbitrary
                      ]

instance Arbitrary L2Reg where
    arbitrary = elements [L2ESI
                         ,L2EDI
                         ,L2EBP
                         ,L2ESP
                         ,L2EDX
                         ,L2EBX
                         ,L2EAX
                         ,L2ECX
                         ]

instance Arbitrary L2Var where
    arbitrary = do
        v <- elements (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
        vs <- listOf $ elements (['_'] ++ ['-'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        return $ L2Var (v:vs)

instance Arbitrary L2AOP where
    arbitrary = elements [L2Add
                         ,L2Sub
                         ,L2Mult
                         ,L2And
                         ]

instance Arbitrary L2SOP where
    arbitrary = elements [L2ShiftLeft
                         ,L2ShiftRight
                         ]

instance Arbitrary L2CMP where
    arbitrary = elements [L2LessThan
                         ,L2LessThanEqual
                         ,L2Equal
                         ]

instance Arbitrary L2Label where
    arbitrary = do
        v <- elements (['_'] ++ ['a'..'z'] ++ ['A'..'Z'])
        vs <- listOf $ elements (['_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        return $ L2Label (v:vs)




