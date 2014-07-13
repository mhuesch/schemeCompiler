module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L2.ParL2
import L2.ErrM
import L2ToL1.Compile
import L1.PrintL1

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
      Ok prog -> do
        case translate prog of
          (Left err) -> putStrLn $ show err
          (Right cp) -> putStrLn . printTree $ cp

