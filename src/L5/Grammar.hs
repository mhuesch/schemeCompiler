module L5.Grammar where


data L5Program = L5Program L5E
                 deriving (Show)

data L5E = L5Lambda [L5X] L5E
         | L5Ex L5X
         | L5Let L5X L5E L5E
         | L5LetRec L5X L5E L5E
         | L5If L5E L5E L5E
         | L5NewTuple [L5E]
         | L5Begin L5E L5E
         | L5Apply L5E [L5E]
         | L5Primitive L5prim
         | L5Enum Int
         deriving (Show)

data L5X = L5X String
           deriving (Show, Eq)

data L5prim = L5pb L5biop
            | L5pp L5pred
            | L5Print
            | L5NewArray
            | L5Aref
            | L5Aset
            | L5Alen
            deriving (Show)

data L5biop = L5Add
            | L5Sub
            | L5Mult
            | L5LT
            | L5LTE 
            | L5Eq
            deriving (Show)

data L5pred = L5IsNumber
            | L5IsA
            deriving (Show)