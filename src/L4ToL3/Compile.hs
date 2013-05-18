module L4ToL3.Compile where


--

import L4.Grammar
import L3.Grammar

translate :: L4Program -> L3Program
translate _ = (L3Program (L3If (L3Vnum 1)
                         (L3Let (L3X "hi") (L3Dv . L3Vnum $ 1) (L3Ed . L3Dv . L3Vnum $ 0))
                         (L3Ed . L3Dv . L3Vnum $ 0))
                    [])