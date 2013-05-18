module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L4.Parser
import L3.Display
import L4ToL3.Compile

main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- liftM readProg $ readFile (args !! 0)
    case result of
        Ok p -> putStrLn . displayProgram . translate $ p
        Failed err -> putStrLn err