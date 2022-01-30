module ASCIIConverterSpec (spec) where

import Test.Hspec
import ASCIIConverter

spec :: Spec
spec = do
  describe "ASCIIConverter" $
    it "Convert Int to ASCII" $
      convert 65 `shouldBe`'A'