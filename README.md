Compiles the following language (L5) to assembly & generates executables

```
e ::= (lambda (x ...) e)
    | x
    | (let ([x e]) e)
    | (letrec ([x e]) e)
    | (if e e e)
    | (new-tuple e ...)
    | (begin e e)
    | (e e ...) ;; application expression
    | prim
    | num
  
prim ::= biop 
       | pred 
       | print
       | new-array
       | aref
       | aset
       | alen

biop ::= + | - | * | < | <= | =
pred ::= number? | a?
```
