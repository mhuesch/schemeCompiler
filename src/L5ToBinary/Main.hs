module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)

import L5.Parser
import L5ToBinary.Compile

main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "usage: filename-to-compile"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    when (not runtimeOExists) $ do
        putStrLn "No runtime.o. Exiting."
        exitFailure
    result <- liftM readProg $ readFile (head args)
    case result of
        Failed err -> putStrLn err
        Ok p -> case translate p of
            Left err -> putStrLn err
            Right assem -> do
                let filename = "prog.S"
                writeFile filename assem
                rawSystem "as" ["--32", "-o", "prog.o", "prog.S"]
                rawSystem "gcc" ["-m32", "-o", "a.out", "prog.o", "runtime.o"]
                return ()