module ASCIIConverter(convertIntToASCII) where

convertIntToASCII :: Int -> Char
convertIntToASCII i = toEnum i :: Char