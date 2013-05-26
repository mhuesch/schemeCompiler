module L4.Display where


import Data.List (intersperse)

import L4.Grammar

displayProgram :: L4Program -> String
displayProgram (L4Program e fs) = "(" ++ displayE e ++ "\n" ++ displayFunctions fs ++ ")"

displayFunctions :: [L4Function] -> String
displayFunctions = concat . intersperse "\n" . map displayFunction

displayFunction :: L4Function -> String
displayFunction (L4Function label xs e) = "(" ++ displayLabel label ++ "\n(" ++ displayXs xs ++ ")\n" ++ displayE e ++ ")"

displayE :: L4E -> String
displayE (L4Let x e1 e2) = "(let ([" ++ displayX x ++ " " ++ displayE e1 ++ "])\n" ++ displayE e2 ++ ")"
displayE (L4If e1 e2 e3) = "(if " ++ displayE e1 ++ "\n" ++ displayE e2 ++ "\n" ++ displayE e3 ++ ")"
displayE (L4Apply e es) = "(" ++ displayEs (e:es) ++ ")"
displayE (L4NewArray e1 e2) = "(new-array " ++ displayEs [e1,e2] ++ ")"
displayE (L4NewTuple es) = "(new-tuple " ++ displayEs es ++ ")"
displayE (L4Aref e1 e2) = "(aref " ++ displayEs [e1,e2] ++ ")"
displayE (L4Aset e1 e2 e3) = "(aset " ++ displayEs [e1,e2,e3] ++ ")"
displayE (L4Alen e1) = "(alen " ++ displayE e1 ++ ")"
displayE (L4Begin e1 e2) = "(begin " ++ displayEs [e1,e2] ++ ")"
displayE (L4Print e1) = "(print " ++ displayE e1 ++ ")"
displayE (L4MakeClosure lab e1) = "(make-closure " ++ displayLabel lab ++ " " ++ displayE e1 ++ ")"
displayE (L4ClosureProc e1) = "(closure-proc " ++ displayE e1 ++ ")"
displayE (L4ClosureVars e1) = "(closure-vars " ++ displayE e1 ++ ")"
displayE (L4Binop bop e1 e2) = "(" ++ displayBiop bop ++ " " ++ displayEs [e1,e2] ++ ")"
displayE (L4Predicate p e1) = "(" ++ displayPred p ++ " " ++ displayE e1 ++ ")"
displayE (L4Enum n) = show n
displayE (L4Ex x) = displayX x
displayE (L4Elab l) = displayLabel l

displayEs :: [L4E] -> String
displayEs = concat . intersperse " " . map displayE


displayX :: L4X -> String
displayX (L4X name) = name

displayXs :: [L4X] -> String
displayXs = concat . intersperse " " . map displayX


displayBiop :: L4biop -> String
displayBiop (L4Add) = "+"
displayBiop (L4Sub) = "-"
displayBiop (L4Mult) = "*"
displayBiop (L4bcmp L4LessThan) = "<"
displayBiop (L4bcmp L4LessThanEqual) = "<="
displayBiop (L4bcmp L4Equal) = "="


displayPred :: L4pred -> String
displayPred L4IsNumber = "number?"
displayPred L4IsA = "a?"

displayLabel :: L4Label -> String
displayLabel (L4Label name) = ':':name



