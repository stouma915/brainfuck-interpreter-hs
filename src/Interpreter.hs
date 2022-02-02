module Interpreter(eval) where

import qualified Data.Map.Strict as M

import Control.Monad
import Data.IORef
import Data.Maybe

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
        '[' -> do
          currentMemory <- readIORef memoryRef
          currentPointer <- readIORef pointerRef
          index <- readIORef indexRef

          let codeBeforeBracket = take index sourceCode
          let codeAfterBracket = drop index sourceCode

          maybeLoopEndIndex <- searchLoopEnd codeBeforeBracket codeAfterBracket
          case maybeLoopEndIndex of
            Just loopEndIndex -> do
              let loopCode = take (loopEndIndex - index) (drop (index + 1) sourceCode)
              let afterLoop = drop (loopEndIndex + 1) sourceCode

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
