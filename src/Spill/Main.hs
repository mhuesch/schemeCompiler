module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L2.AbsL2
import L2.ParL2
import L2.PrintL2
import L2.ErrM
import Spill.Spill


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "usage: filename"
        exitFailure
    ts <- liftM myLexer $ readFile (head args)
    case pSpill ts of
      Bad s -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        print ts
        putStrLn s
      Ok (Sp instrs varToSpill (PosNegInteger offset) prefix) -> do
        let res = spill instrs varToSpill (read offset) prefix
        putStrLn "("
        putStrLn $ printTree res
        putStrLn ")"

