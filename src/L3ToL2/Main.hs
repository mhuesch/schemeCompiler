module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L3.Parser
import L2.Display
import L3ToL2.Compile

main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- liftM readProg (readFile (args !! 0))
    case result of
        Ok p -> putStrLn . displayProgram . translate $ p
        Failed err -> putStrLn err
