module Main where


import Control.Monad
import System.Environment
import System.IO
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)
import Text.ParserCombinators.Parsec hiding (State)

import L1.Parser
import L1Tox86.Compile

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    when (not runtimeOExists) $ putStrLn "No runtime.o. Exiting."
    contents <- liftM readProg $ readFile (args !! 0)
    let filename = "prog.S"
        prog = generateAssembly contents
    writeFile filename prog
    rawSystem "as" ["--32", "-o", "prog.o", "prog.S"]
    rawSystem "gcc" ["-m32", "-o", "a.out", "prog.o", "runtime.o"]
    return ()


