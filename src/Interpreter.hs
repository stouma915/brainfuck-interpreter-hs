module Interpreter(eval) where

import qualified Data.Map.Strict as M

import System.IO

import Control.Monad
import Data.IORef
import Data.Maybe
import Text.Read

import ASCIIConverter
import BrainfuckException
import Util

eval :: String -> Int -> [(Int, Int)] -> IO (String, (Int, [(Int, Int)]))
eval sourceCode memoryPointer memoryEntries = do
  memoryRef <- newIORef $ M.fromList memoryEntries
  pointerRef <- newIORef memoryPointer
  outputRef <- newIORef ""

  indexRef <- newIORef (0 :: Int)
  stopRef <- newIORef False

  forM_ sourceCode $ \char -> do
    stop <- readIORef stopRef
    if stop then
      pure ()
    else do
      case char of
        '+' -> do
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef

          if isNothing (currentMemory M.!? currentPointer) then
            modifyIORef memoryRef (M.insert currentPointer 1)
          else do
            let currentValue = currentMemory M.! currentPointer

            if currentValue >= 255 then
              modifyIORef memoryRef (M.update (\_ -> Just 0) currentPointer)
            else
              modifyIORef memoryRef (M.update (\x -> Just(x + 1)) currentPointer)
        '-' -> do
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef

          if isNothing (currentMemory M.!? currentPointer) then
            modifyIORef memoryRef (M.insert currentPointer 255)
          else do
            let currentValue = currentMemory M.! currentPointer

            if currentValue <= 0 then
              modifyIORef memoryRef (M.update (\_ -> Just 255) currentPointer)
            else
              modifyIORef memoryRef (M.update (\x -> Just(x - 1)) currentPointer)
        '>' ->
          modifyIORef pointerRef (+ 1)
        '<' -> do
          currentPointer <- readIORef pointerRef

          if currentPointer >= 1 then
            modifyIORef pointerRef (\x -> x - 1)
          else
            pure ()
        '.' -> do
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef

          if isNothing (currentMemory M.!? currentPointer) then
            modifyIORef outputRef (\x -> x ++ ['\0'])
          else do
            let currentValue = currentMemory M.! currentPointer
            let converted = convertIntToASCII currentValue

            modifyIORef outputRef (\x -> x ++ [converted])
        ',' -> do
          doneRef <- newIORef False
          inputRef <- newIORef (0 :: Int)

          while (lazyCompare doneRef False) (\_ -> do
            putStr "Input was requested: "
            hFlush stdout

            line <- getLine

            let maybeInput = readMaybe line :: Maybe Int
            case maybeInput of
              Just i -> do
                if i >= -128 && i <= 127 then do
                  writeIORef inputRef i
                  writeIORef doneRef True
                else
                  putStrLn "Please enter a 1 byte number."
              Nothing -> do
                putStrLn "Please enter a 1 byte number."
            )

          input <- readIORef inputRef
          
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef
          if isNothing (currentMemory M.!? currentPointer) then
            modifyIORef memoryRef (M.insert currentPointer 0)
          else
            modifyIORef memoryRef (M.update (\_ -> Just 0) currentPointer)

          forM_ [1..abs input] $ \_ -> do
            mem <- readIORef memoryRef
            pointer <- readIORef pointerRef
            if input < 0 then
              if isNothing (mem M.!? pointer) then
                modifyIORef memoryRef (M.insert pointer 255)
              else do
                let currentValue = mem M.! pointer

                if currentValue <= 0 then
                  modifyIORef memoryRef (M.update (\_ -> Just 255) pointer)
                else
                  modifyIORef memoryRef (M.update (\x -> Just(x - 1)) pointer)
            else
              if isNothing (mem M.!? pointer) then
                modifyIORef memoryRef (M.insert pointer 1)
              else do
                let currentValue = mem M.! pointer

                if currentValue >= 255 then
                  modifyIORef memoryRef (M.update (\_ -> Just 0) pointer)
                else
                  modifyIORef memoryRef (M.update (\x -> Just(x + 1)) pointer)
        '[' -> do
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef
          index <- readIORef indexRef

          let codeBeforeBracket = take index sourceCode
          let codeAfterBracket = drop index sourceCode

          maybeLoopEndIndex <- searchLoopEnd codeBeforeBracket codeAfterBracket
          case maybeLoopEndIndex of
            Just loopEndIndex -> do
              let loopCode = take (loopEndIndex - 1 - index) (drop (index + 1) sourceCode)
              let afterLoop = drop loopEndIndex sourceCode

              currentValueRef <- newIORef (0 :: Int)
              writeIORef currentValueRef $ do
                if isNothing (currentMemory M.!? currentPointer) then
                  0
                else do
                  let value = currentMemory M.! currentPointer
                  value

              while (lazyCompareNot currentValueRef 0) (\_ -> do
                mem <- readIORef memoryRef
                point <- readIORef pointerRef

                evalResult <- eval loopCode point (M.toList mem)
                let newMemory = snd (snd evalResult)
                let newPointer = fst (snd evalResult)
                let output = fst evalResult

                writeIORef memoryRef (M.fromList newMemory)
                writeIORef pointerRef newPointer
                modifyIORef outputRef (++ output)

                writeIORef currentValueRef $ do
                  if isNothing (M.fromList newMemory M.!? newPointer) then
                    0
                  else do
                    let value = M.fromList newMemory M.! newPointer
                    value

                pure ()
                )

              mem <- readIORef memoryRef
              point <- readIORef pointerRef

              evalResult <- eval afterLoop point (M.toList mem)
              let newMemory = snd (snd evalResult)
              let newPointer = fst (snd evalResult)
              let output = fst evalResult

              writeIORef memoryRef (M.fromList newMemory)
              writeIORef pointerRef newPointer
              modifyIORef outputRef (++ output)

              writeIORef stopRef True
            Nothing ->
              brainfuckError "The end of the loop couldn't be identified."
        _ ->
          pure ()

    modifyIORef indexRef (+ 1)

  memory <- readIORef memoryRef
  pointer <- readIORef pointerRef
  output <- readIORef outputRef

  return (output, (pointer, M.toList memory))
