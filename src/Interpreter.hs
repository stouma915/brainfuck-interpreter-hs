module Interpreter(eval) where

import Control.Monad
import Data.IORef

eval :: String -> Int -> [(Int, Int)] -> IO String
eval sourceCode memoryPointer memoryEntries = do
  pointerRef <- newIORef memoryPointer

  forM_ sourceCode $ \char -> do
    case char of
      '>' -> do
        modifyIORef pointerRef (+ 1)
      '<' -> do
        currentPointer <- readIORef pointerRef

        if currentPointer >= 1 then
          modifyIORef pointerRef (\x -> x - 1)
        else
          pure ()
      _ -> do 
        pure ()

  return ""