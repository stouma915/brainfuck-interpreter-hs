module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)
import System.IO ()

import Control.Exception (try)
import GHC.IO.Exception (ioe_description)

import Interpreter (eval)
import BrainfuckException (BrainfuckError)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName

  if length args /= 1 then do
    showHelp
  else do
    let sourceFileName = head args

    if head sourceFileName == '-' then
      case sourceFileName of
        "--help" -> showHelp
        "-h" -> showHelp
        _ -> do
          putStrLn $ "Found argument '" ++ sourceFileName ++ "' which wasn't expected, or isn't valid in this context."
          putStrLn ""
          putStrLn "Usage:"
          putStrLn $ "    " ++ progName ++  " [FLAGS] [SOURCE]"
          putStrLn ""
          putStrLn "For more information try --help"
          
          exitWith (ExitFailure 2)
    else do
      contentOrErr <- try (readFile sourceFileName) :: IO (Either IOError String)
      case contentOrErr of
        Left err -> do
          putStrLn $ "Unable to read source file: " ++ ioe_description err

          exitWith (ExitFailure 2)
        Right content -> do
          let replaced = map(\c -> if c == '\n' then ' '; else c) content

          resultOrErr <- try (eval replaced 0 [(0, 0)]) :: IO (Either BrainfuckError (String, (Int, [(Int, Int)])))
          case resultOrErr of
            Left err -> do
              putStrLn "Evaluate Failed."
              print err

              exitWith (ExitFailure 1)
            Right result -> do
              putStrLn $ fst result

              exitSuccess

showHelp :: IO ()
showHelp = do
  progName <- getProgName

  putStrLn "Brainf**k Interpreter 1.0.0"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn $ "    " ++ progName ++  " [FLAGS] [SOURCE]"
  putStrLn ""
  putStrLn "Flags:"
  putStrLn "    -h, --help    Prints help information."
  putStrLn ""
  putStrLn "Args:"
  putStrLn "<SOURCE> Brainfuck source file."