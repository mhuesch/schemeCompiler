{
module L3.Parser where
import L3.Token
import L3.Grammar

}

%name calc
%tokentype { Token }
%error { parseError }

%token
    '('     { TOpen }
    ')'     { TClose }

    let         { TLet }
    if          { TIf }
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

p       : '(' e funs ')'         { L3Program $2 $3 }

funs    : {- empty -} { [] }
        | funs fun    { $2 : $1 }

fun     : '(' label '(' xs ')' e ')'    { L3Function $2 $4 $6}

e       : '(' let '(' xd_pairs ')' e ')'  { L3Let $4 $6 }
        | '(' if v e e ')'                { L3If $3 $4 $5 }
        | d                               { L3Ed $1 }

d       : '(' '+' v v ')'       { L3Binop L3Add $3 $4 }
        | '(' '-' v v ')'       { L3Binop L3Sub $3 $4 }
        | '(' '*' v v ')'       { L3Binop L3Mult $3 $4 }
        | '(' '<' v v ')'       { L3Binop L3LessThan $3 $4 }
        | '(' "<=" v v ')'      { L3Binop L3LessThanEqual $3 $4 }
        | '(' '=' v v ')'       { L3Binop L3Equal $3 $4 }
        | '(' isA v ')'         { L3Predicate L3IsA $3 }
        | '(' isNumber v ')'    { L3Predicate L3IsNumber $3 }
        | '(' v vs ')'          { L3Apply $2 $3 }
        | '(' newArray v v ')'  { L3NewArray $3 $4 }
        | '(' newTuple vs ')'   { L3NewTuple $3 }
        | '(' aref v v ')'      { L3Aref $3 $4 }
        | '(' aset v v v ')'    { L3Aset $3 $4 $5 }
        | '(' alen v ')'        { L3Alen $3 }
        | '(' print v ')'       { L3Print $3 }
        | '(' makeClosure label v ')' { L3MakeClosure $3 $4 }
        | '(' closureProc v ')' { L3ClosureProc $3 }
        | '(' closureVars v ')' { L3ClosureVars $3 }
        | v                     { L3Dv $1 }

label   : lab      { L3Label $1 }

v       : x        { L3Vx $1 }
        | label    { L3Vlab $1 }
        | int      { L3Vnum $1 }

x       : var      { L3X $1 }

vs      : {- empty -} { [] }
        | vs v        { $2 : $1 }

xd_pair : '(' x d ')'      { ($2, $3) }

xd_pairs : {- empty -}      { [] }
         | xd_pairs xd_pair { $2 : $1 }

xs      : {- empty -} { [] }
        | xs x        { $2 : $1 }



{
parseError :: [Token] -> a
parseError msg = error $ show msg


readProg = calc . lexer
}

