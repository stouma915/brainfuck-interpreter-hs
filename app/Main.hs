module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)
import System.IO ()

import Control.Exception (try)
import GHC.IO.Exception (ioe_description)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  if null args then do
    putStrLn "Brainf**k Interpreter 1.0.0"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn $ "    " ++ progName ++  " [SOURCE]"
    putStrLn ""
    putStrLn "Args:"
    putStrLn "<SOURCE> Brainfuck source file."
  else do
    let sourceFileName = head args

    contentOrErr <- try (readFile sourceFileName) :: IO (Either IOError String)
    case contentOrErr of
      Left err -> do
        putStrLn $ "Unable to read source file: " ++ ioe_description err

        exitWith (ExitFailure 2)
      Right content -> do
        let replaced = map(\c -> if c == '\n' then ' '; else c) content
        
        putStrLn replaced
        
        exitSuccess
