module LibSpec where

import Lib
import Test.Hspec

spec :: Spec
spec =
  describe "LibSpec.returnThree" $ do
    it "returns three for a string" $ do
      returnThree "sab" `shouldBe` (3 :: Int)

    it "returns three for a char" $ do
      returnThree 'e' `shouldBe` (3 :: Int)