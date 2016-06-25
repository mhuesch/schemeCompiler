module Main where


import Control.Monad
import System.Exit (exitFailure)
import System.Environment

import L2.AbsL
import L2.ParL
import L2.ErrM
import Liveness.Liveness
import Graph.GraphGlue
import Graph.Interference
import Graph.Color


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
      putStrLn "usage: filename"
      exitFailure
    ts <- liftM myLexer $ readFile (head args)
    case pParenListInstruction ts of
      Bad s -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        print ts
        putStrLn s
      Ok (PLI is) -> do
        let iG = buildInterference . liveRes $ is
            coloring = runColor iG
        putStrLn . displayIGraph $ iG
        putStrLn . displayColors $ coloring

