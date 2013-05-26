{
module L5.Token where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  \;.*                  ;

  [\( \{ \[ ]           { \s -> TOpen }
  [\) \} \] ]           { \s -> TClose }

  lambda                { \s -> TLambda }

  let                   { \s -> TLet }
  letrec                { \s -> TLetRec }
  if                    { \s -> TIf }
  print                 { \s -> TPrint }
  begin                 { \s -> TBegin }
  "new-array"           { \s -> TNewArray }
  "new-tuple"           { \s -> TNewTuple }
  aref                  { \s -> TAref }
  aset                  { \s -> TAset }
  alen                  { \s -> TAlen }

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

  [$alpha \_] [$alpha $digit \_ \-]*   { \s -> TVar s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    TOpen        |
    TClose       |

    TLambda      |
    
    TLet         |
    TLetRec      |
    TIf          |
    TBegin       |
    TPrint       |
    TNewArray    |
    TNewTuple    |
    TAref        |
    TAset        |
    TAlen        |

    TInt Int     |
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