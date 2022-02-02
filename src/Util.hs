module Util(searchLoopEnd) where

import Control.Monad
import Data.IORef

searchLoopEnd :: String -> String -> IO (Maybe Int)
searchLoopEnd before after = do
  resultRef <- newIORef (-1 :: Int)

  countOfBracketRef <- newIORef (0 :: Int)
  countOfClosingBracketRef <- newIORef (0 :: Int)

  indexRef <- newIORef (0 :: Int)

  forM_ after $ \char -> do
    result <- readIORef resultRef
    
    if result == -1 then do
      case char of
        '[' ->
          modifyIORef countOfBracketRef (+ 1)
        ']' ->
          modifyIORef countOfClosingBracketRef (+ 1)
        _ ->
          pure ()

      countOfBracket <- readIORef countOfBracketRef
      countOfClosingBracket <- readIORef countOfClosingBracketRef

      if countOfBracket == countOfClosingBracket then do
        index <- readIORef indexRef

        let loopEndIndex = length before + index

        writeIORef resultRef loopEndIndex
      else
        pure ()
    else
      pure ()
        
    modifyIORef indexRef (+ 1)

  result <- readIORef resultRef

  if result == -1 then
    pure Nothing
  else
    pure $ Just result