{
module L5.Parser where


import Control.Monad.Error

import L5.Token
import L5.Grammar
}

%name calc
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
    '('     { TOpen }
    ')'     { TClose }

    lambda      { TLambda }

    let         { TLet }
    letrec      { TLetRec }
    if          { TIf }
    begin       { TBegin }
    print       { TPrint }

    newArray   { TNewArray }
    newTuple   { TNewTuple }
    aref        { TAref }
    aset        { TAset }
    alen        { TAlen }

    int         { TInt $$ }
    var         { TVar $$ }

    isA          { TIsA }
    isNumber     { TIsNumber }
    '+'          { TAdd }
    '-'          { TSub }
    '*'          { TMult }
    '<'          { TLT }
    "<="         { TLTE }
    '='          { TEq }

%%

p       : e         { L5Program $1 }

e       : '(' lambda '(' xs ')' e ')'           { L5Lambda (reverse $4) $6 }
        | x                                     { L5Ex $1 }
        | '(' let '(' '(' x e ')' ')' e ')'     { L5Let $5 $6 $9 }
        | '(' letrec '(' '(' x e ')' ')' e ')'  { L5LetRec $5 $6 $9 }
        | '(' if e e e ')'                      { L5If $3 $4 $5 }
        | '(' newTuple es ')'                   { L5NewTuple (reverse $3) }
        | '(' begin e e ')'                     { L5Begin $3 $4 }
        | '(' e es ')'                          { L5Apply $2 (reverse $3) }
        | prim                                  { L5Primitive $1 }
        | int                                   { L5Enum $1 }

es      : {- empty -} { [] }
        | es e        { $2 : $1 }


prim    : biop              { L5pb $1 }
        | pred              { L5pp $1 }
        | print             { L5Print }
        | newArray          { L5NewArray }
        | aref              { L5Aref }
        | aset              { L5Aset }
        | alen              { L5Alen }

biop    : '+'   { L5Add }
        | '-'   { L5Sub }
        | '*'   { L5Mult }
        | '<'   { L5LT }
        | "<="  { L5LTE }
        | '='   { L5Eq }

pred    : isNumber  { L5IsNumber }
        | isA       { L5IsA }


x       : var   { L5X $ '_':$1 }

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








