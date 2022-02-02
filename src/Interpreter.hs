module Interpreter(eval) where

import qualified Data.Map.Strict as M

import Control.Monad
import Data.IORef
import Data.Maybe

eval :: String -> Int -> [(Int, Int)] -> IO String
eval sourceCode memoryPointer memoryEntries = do
  memoryRef <- newIORef $ M.fromList memoryEntries
  pointerRef <- newIORef memoryPointer

  forM_ sourceCode $ \char -> do
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
      _ ->
        pure ()

  return ""