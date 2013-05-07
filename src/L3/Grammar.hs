module L3.Grammar where


data L3Program = L3Program L3E [L3Function]
                 deriving (Show)

data L3Function = L3Function L3Label [L3X] L3E
                  deriving (Show)

data L3E = L3Let [(L3X,L3D)] L3E
         | L3If L3V L3E L3E
         | L3Ed L3D
         deriving (Show)

data L3D = L3Binop L3biop L3V L3V
         | L3Predicate L3pred L3V
         | L3Apply L3V [L3V]
         | L3NewArray L3V L3V
         | L3NewTuple [L3V]
         | L3Aref L3V L3V
         | L3Aset L3V L3V L3V
         | L3Alen L3V
         | L3Print L3V
         | L3MakeClosure L3Label L3V
         | L3ClosureProc L3V
         | L3ClosureVars L3V
         | L3Dv L3V
         deriving (Show)

data L3V = L3Vx L3X
         | L3Vlab L3Label
         | L3Vnum Int
         deriving (Show)

data L3biop = L3Add
            | L3Sub
            | L3Mult
            | L3LessThan
            | L3LessThanEqual
            | L3Equal
            deriving (Show)

data L3pred = L3IsNumber
            | L3IsA
            deriving (Show)

data L3Label = L3Label String
               deriving (Show)

data L3X = L3X String
           deriving (Show)

