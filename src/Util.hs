module Util(searchLoopEnd, while, lazyCompare, lazyCompareNot) where

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

        let loopEndIndex = length before + (index + 1)

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

while :: IO Bool -> (() -> IO ()) -> IO ()
while condition content = do
  cond <- condition
  if cond then do
    content ()
    while condition content
  else
    pure ()

lazyCompare :: (Eq a) => IORef a -> a -> IO Bool
lazyCompare ref target = do
  content <- readIORef ref
  return (content == target)

lazyCompareNot :: (Eq a) => IORef a -> a -> IO Bool
lazyCompareNot ref target = do
  c <- lazyCompare ref target

  if c then
    pure False
  else
    pure True
