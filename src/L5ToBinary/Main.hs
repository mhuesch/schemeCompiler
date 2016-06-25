module Main where


import Control.Monad
import System.Exit
import System.Environment
import System.Directory (doesFileExist)
import System.Cmd (rawSystem)

import L5.ParL5
import L5.ErrM
import L5ToBinary.Compile

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "usage: filename-to-compile"
        exitFailure
    runtimeOExists <- doesFileExist "runtime.o"
    when (not runtimeOExists) $ do
        putStrLn "No runtime.o. Exiting."
        exitFailure
    ts <- liftM myLexer $ readFile (head args)
    case pProgram ts of
      Bad s -> do
        putStrLn "\nParse              Failed...\n"
        putStrLn "Tokens:"
        print ts
        putStrLn s
      Ok prog -> case translate prog of
        Left err -> putStrLn err
        Right assem -> do
          let filename = "prog.S"
          writeFile filename assem
          rawSystem "as" ["--64", "-o", "prog.o", "prog.S"] >>= throwIfError
          rawSystem "gcc" ["-m64", "-o", "a.out", "prog.o", "runtime.o"] >>= throwIfError
        

throwIfError :: ExitCode -> IO ()
throwIfError ExitSuccess = return ()
throwIfError e@ExitFailure{} = void (exitWith e)

