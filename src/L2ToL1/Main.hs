module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

import L2.Parser
import L2ToL1.Compile
import L1.Display

main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- parseFromFile parseProg (args !! 0)
    case result of
        Left err -> putStrLn . show $ err
        Right p -> do
            case translate p of
                (Left err) -> putStrLn $ show err
                (Right cp) -> putStrLn . displayProgram $ cp