{
module L4.Parser where


import Control.Monad.Error

import L4.Token
import L4.Grammar
}

%name calc
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
    '('     { TOpen }
    ')'     { TClose }

    let         { TLet }
    if          { TIf }
    begin       { TBegin }
    print       { TPrint }

    newArray   { TNewArray }
    newTuple   { TNewTuple }
    aref        { TAref }
    aset        { TAset }
    alen        { TAlen }
    makeClosure { TMakeClosure }
    closureProc { TClosureProc }
    closureVars { TClosureVars }

    int         { TInt $$ }
    var         { TVar $$ }
    lab         { TLab $$ }

    isA          { TIsA }
    isNumber     { TIsNumber }
    '+'          { TAdd }
    '-'          { TSub }
    '*'          { TMult }
    '<'          { TLT }
    "<="         { TLTE }
    '='          { TEq }

%%

p       : '(' e funs ')'         { L4Program $2 $3 }

funs    : {- empty -} { [] }
        | funs fun    { $2 : $1 }

fun     : '(' label '(' xs ')' e ')'     { L4Function $2 (reverse $4) $6}

e       : '(' let '(' '(' x e ')' ')' e ')'  { L4Let $5 $6 $9 }
        | '(' if e e e ')'                   { L4If $3 $4 $5 }
        | '(' bop e e ')'                    { L4Binop $2 $3 $4 }
        | '(' isA e ')'                      { L4Predicate L4IsA $3 }
        | '(' isNumber e ')'                 { L4Predicate L4IsNumber $3 }
        | '(' newArray e e ')'               { L4NewArray $3 $4 }
        | '(' newTuple es ')'                { L4NewTuple (reverse $3) }
        | '(' aref e e ')'                   { L4Aref $3 $4 }
        | '(' aset e e e ')'                 { L4Aset $3 $4 $5 }
        | '(' alen e ')'                     { L4Alen $3 }
        | '(' begin e e ')'                  { L4Begin $3 $4 }
        | '(' print e ')'                    { L4Print $3 }
        | '(' makeClosure label e ')'        { L4MakeClosure $3 $4 }
        | '(' closureProc e ')'              { L4ClosureProc $3 }
        | '(' closureVars e ')'              { L4ClosureVars $3 }
        | '(' e es ')'                       { L4Apply $2 (reverse $3) }
        | int                                { L4Enum $1 }
        | x                                  { L4Ex $1 }
        | label                              { L4Elab $1 }

bop     : '+'   { L4Add }
        | '-'   { L4Sub }
        | '*'   { L4Mult }
        | '<'   { L4bcmp L4LessThan }
        | "<="  { L4bcmp L4LessThanEqual }
        | '='   { L4bcmp L4Equal }

es      : {- empty -} { [] }
        | es e        { $2 : $1 }

label   : lab   { L4Label $ '_':$1 }

x       : var   { L4X $ '_':$1 }

xs      : {- empty -} { [] }
        | xs x        { $2 : $1 }



{
data E a = Ok a
         | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = case m of 
    Ok a -> k a
    Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
    Ok a -> Ok a
    Failed e -> k e


parseError :: [Token] -> E a
parseError tokens = failE $ "Parse error. Tokens: " ++ show tokens


dropEnd :: [Token] -> [Token]
dropEnd [] = []
dropEnd (t:ts) = case t of
    TOpen -> t : matchClose 1 ts
    _ -> t : dropEnd ts

matchClose :: Int -> [Token] -> [Token]
matchClose _ [] = []
matchClose 0 ts = []
matchClose i (t:ts) = case t of
    TOpen -> t : matchClose (i + 1) ts
    TClose -> t : matchClose (i - 1) ts
    _ -> t : matchClose i ts

readProg = calc . dropEnd . lexer
}








