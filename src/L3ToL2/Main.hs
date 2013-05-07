module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L3.Grammar
import L3.Parser

main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- liftM readProg (readFile (args !! 0))
    putStrLn . show $ result
