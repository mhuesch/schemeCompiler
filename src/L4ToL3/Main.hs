module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L4.ParL4
import L4.ErrM
import L4ToL3.Compile
import L3.PrintL3

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

