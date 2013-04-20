module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

import L2.Grammar
import L2.Parser
import L2.Display
import Liveness.Liveness


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- parseFromFile parseLiveness (args !! 0)
    case result of
        Left err -> putStrLn . show $ err
        Right ls -> putStrLn . displayLiveArray . convergeLiveArray . liveListToArray $ ls


