module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment
import Text.ParserCombinators.Parsec hiding (State)

import L2.Parser
import Liveness.Liveness
import Graph.GraphGlue
import Graph.Interference
import Graph.Color


main :: IO ()
main = do
    args <- getArgs
    when (length args == 0) $ do
        putStrLn "usage: filename"
        exitFailure
    result <- parseFromFile parseFunBody (args !! 0)
    case result of
        Left err -> putStrLn . show $ err
        Right ls -> do
            let iG = buildInterference . liveRes $ ls
                coloring = runColor iG
            putStrLn . displayIGraph $ iG
            putStrLn . displayColors $ coloring


