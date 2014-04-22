module Main where


import Control.Monad
import System.Environment
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)

import L1_64.ParL1
import L1_64.ErrM

import L1Tox64.Compile

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "usage: filename"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    unless runtimeOExists $ putStrLn "No runtime.o. Exiting." >> exitFailure
    contents <- readFile (head args)
    let ts = myLexer contents
    case pProgram ts of
      Bad s   -> do putStrLn "\nParse              Failed...\n"
                    putStrLn "Tokens:"
                    print ts
                    putStrLn s
      Ok tree -> do writeFile "prog.S" (generateAssembly tree)
                    rawSystem "as" ["--64", "-o", "prog.o", "prog.S"]
                    rawSystem "gcc" ["-m64", "-o", "a.out", "prog.o", "runtime.o"]
                    return ()


