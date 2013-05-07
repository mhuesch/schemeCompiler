{
module L3.Token where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  \;.*                  ;

  [\( \{ \[ ]           { \s -> TOpen }
  [\) \} \] ]           { \s -> TClose }

  let                   { \s -> TLet }
  if                    { \s -> TIf }
  print                 { \s -> TPrint }
  "new-array"           { \s -> TNewArray }
  "new-tuple"           { \s -> TNewTuple }
  aref                  { \s -> TAref }
  aset                  { \s -> TAset }
  alen                  { \s -> TAlen }
  "make-closure"        { \s -> TMakeClosure }
  "closure-proc"        { \s -> TClosureProc }
  "closure-vars"        { \s -> TClosureVars }

  \- $digit+            { \s -> TInt (read s) }
  $digit+               { \s -> TInt (read s) }

  "a?"                  { \s -> TIsA }
  "number?"             { \s -> TIsNumber }
  \+                    { \s -> TAdd }
  \-                    { \s -> TSub }
  \*                    { \s -> TMult }
  \<                    { \s -> TLT }
  "<="                  { \s -> TLTE }
  \=                    { \s -> TEq }

  \: [$alpha \_] [$alpha $digit \_]*   { \s -> TLab (tail s) }
  [$alpha \_] [$alpha $digit \_ \-]*   { \s -> TVar s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    TOpen        |
    TClose       |

    TLet         |
    TIf          |
    TPrint       |
    TNewArray    |
    TNewTuple    |
    TAref        |
    TAset        |
    TAlen        |
    TMakeClosure |
    TClosureProc |
    TClosureVars |

    TInt Int     |
    TLab String  |
    TVar String  |

    TIsA         |
    TIsNumber    |
    TAdd         |
    TSub         |
    TMult        |
    TLT          |
    TLTE         |
    TEq          

    deriving (Eq,Show)

lexer = alexScanTokens
}