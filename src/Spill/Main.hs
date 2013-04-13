module Spill.Main where

import Control.Monad
import System.Exit (exitFailure)
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

import L2.Grammar
import L2.Parser
import L2.Display


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    spillFile (args !! 0)

spillFile :: FilePath -> IO ()
spillFile fp = do
    result <- parseFromFile parseSpill fp
    case result of
        Left err -> putStrLn . show $ err
        Right sp -> putStrLn . displayInstrList . spillSpill $ sp


spillSpill :: L2Spill -> [L2Instruction]
spillSpill (L2Spill ls var offset prefix) = ls
