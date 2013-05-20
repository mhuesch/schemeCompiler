module L3.Display where


import Data.List (intersperse)

import L3.Grammar

displayProgram :: L3Program -> String
displayProgram (L3Program e fs) = "(" ++ displayE e ++ "\n" ++ displayFunctions fs ++ ")"

displayFunctions :: [L3Function] -> String
displayFunctions = concat . intersperse "\n" . map displayFunction

displayFunction :: L3Function -> String
displayFunction (L3Function label xs e) = "(" ++ displayLabel label ++ "\n" ++ displayXList xs ++ "\n" ++ displayE e ++ ")" 

displayE :: L3E -> String 
displayE (L3Let x d e) = "(let ([" ++ displayX x ++ " " ++ displayD d ++ "])\n" ++ displayE e ++ ")"
displayE (L3If v e1 e2) = "(if " ++ displayV v ++ "\n" ++ displayE e1 ++ "\n" ++ displayE e2 ++ ")"
displayE (L3Ed d) = displayD d

displayD :: L3D -> String
displayD (L3Binop bop v1 v2) = "(" ++ displayBiop bop ++ " " ++ displayV v1 ++ " " ++ displayV v2 ++ ")"
displayD (L3Predicate p v) = "(" ++ displayPred p ++ " " ++ displayV v ++ ")"
displayD (L3Apply v vs) = "(" ++ displayVs (v:vs) ++ ")"
displayD (L3NewArray v1 v2) = "(new-array " ++ displayVs [v1,v2] ++ ")"
displayD (L3NewTuple vs) = "(new-tuple " ++ displayVs vs ++ ")"
displayD (L3Aref v1 v2) = "(aref " ++ displayVs [v1,v2] ++ ")"
displayD (L3Aset v1 v2 v3) = "(aset " ++ displayVs [v1,v2,v3] ++ ")"
displayD (L3Print v) = "(print " ++ displayV v ++ ")"
displayD (L3MakeClosure lab v) = "(make-closure " ++ displayLabel lab ++ " " ++ displayV v ++ ")"
displayD (L3ClosureProc v) = "(closure-proc " ++ displayV v ++ ")"
displayD (L3ClosureVars v) = "(closure-vars " ++ displayV v ++ ")"
displayD (L3Dv v) = displayV v


displayV :: L3V -> String
displayV (L3Vx x) = displayX x
displayV (L3Vlab l) = displayLabel l
displayV (L3Vnum n) = show n

displayVs :: [L3V] -> String
displayVs = concat . intersperse " " . map displayV

displayX :: L3X -> String
displayX (L3X name) = name

displayXList :: [L3X] -> String
displayXList xs = "(" ++ (concat . intersperse " " $ map displayX xs) ++ ")"

displayBiop :: L3biop -> String
displayBiop L3Add = "+"
displayBiop L3Sub = "-"
displayBiop L3Mult = "*"
displayBiop L3LessThan = "<"
displayBiop L3LessThanEqual = "<="
displayBiop L3Equal = "="

displayPred :: L3pred -> String
displayPred L3IsNumber = "number?"
displayPred L3IsA = "a?"


displayLabel :: L3Label -> String
displayLabel (L3Label name) = ':':name