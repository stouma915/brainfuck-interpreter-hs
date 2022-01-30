module ASCIIConverter( convert ) where

convert :: Int -> Char
convert i = toEnum i :: Char