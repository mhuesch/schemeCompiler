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

# To build
1. Install [BNFC](http://bnfc.digitalgrammars.com/).
  * `cabal install bnfc` works.
  * Ensure that `bnfc` is on your path.
2. From the project root, run `./src/genAll.sh`
3. `cabal install`
4. ???
5. Profit
