module Main where


import Control.Monad
import System.Environment
import System.Cmd
import System.Exit
import System.Directory

import L1.ParL
import L1.ErrM

import L1Tox64.Compile
import Paths_schemeCompiler

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "usage: filename"
        exitFailure
    runtimeOPath <- getDataFileName "data/runtime.o"
    contents <- readFile (head args)
    let ts = myLexer contents
    case pProgram ts of
      Bad s   -> do putStrLn "\nParse              Failed...\n"
                    putStrLn "Tokens:"
                    print ts
                    putStrLn s
      Ok tree -> do writeFile "prog.S" (assembleProgram tree)
                    rawSystem "as" ["--64", "-o", "prog.o", "prog.S"] >>= throwIfError
                    rawSystem "gcc" ["-m64", "-o", "a.out", "prog.o", runtimeOPath] >>= throwIfError

throwIfError :: ExitCode -> IO ()
throwIfError ExitSuccess = return ()
throwIfError e@ExitFailure{} = void (exitWith e)

