module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L3.ParL
import L3.ErrM
import L3ToL2.Compile
import L2.PrintL

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
      putStrLn "usage: filename"
      exitFailure
    ts <- liftM myLexer $ readFile (head args)
    case pProgram ts of
      Bad s -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        print ts
        putStrLn s
      Ok prog -> putStrLn . printTree . translate $ prog

