module BrainfuckException(BrainfuckError, BrainfuckException, brainfuckError) where

import Control.Exception
import Data.Data (Typeable)

type BrainfuckError = BrainfuckException

newtype BrainfuckException = BrainfuckError{description :: String}
                               deriving (Show, Typeable)

instance Exception BrainfuckException

brainfuckError :: String -> IO a
brainfuckError msg = throwIO (BrainfuckError msg)
