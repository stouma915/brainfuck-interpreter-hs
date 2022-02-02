module ASCIIConverterSpec (spec) where

import Test.Hspec
import ASCIIConverter

spec :: Spec
spec = do
  describe "ASCIIConverter" $
    it "Convert Int to ASCII" $
      convertIntToASCII 65 `shouldBe`'A'
