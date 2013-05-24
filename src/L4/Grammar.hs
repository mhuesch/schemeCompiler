module L4.Grammar where


data L4Program = L4Program L4E [L4Function]
                 deriving (Show)

data L4Function = L4Function L4Label [L4X] L4E
                  deriving (Show)

data L4E = L4Let L4X L4E L4E
         | L4If L4E L4E L4E
         | L4Apply L4E [L4E]
         | L4NewArray L4E L4E
         | L4NewTuple [L4E]
         | L4Aref L4E L4E
         | L4Aset L4E L4E L4E
         | L4Alen L4E
         | L4Begin L4E L4E
         | L4Print L4E
         | L4MakeClosure L4Label L4E
         | L4ClosureProc L4E
         | L4ClosureVars L4E
         | L4Binop L4biop L4E L4E
         | L4Predicate L4pred L4E
         | L4Enum Int
         | L4Ex L4X
         | L4Elab L4Label
         deriving (Show)

data L4biop = L4Add
            | L4Sub
            | L4Mult
            | L4bcmp L4CMP
            deriving (Show)

data L4CMP = L4LessThan
           | L4LessThanEqual
           | L4Equal
           deriving (Show)

data L4pred = L4IsNumber
            | L4IsA
            deriving (Show)

data L4Label = L4Label String
               deriving (Show)

data L4X = L4X String
           deriving (Show, Ord, Eq)