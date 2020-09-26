module Parser.ParserSpec where
import Parser.Parser
import Parser.ParserData
import Control.Monad.State.Lazy
import Test.Hspec

spec :: Spec
spec =
    describe "ParserSpec" $ do
        it "Parses p1 correct" $ do
            parseScheme p1 `shouldBe` n1
        it "Parses p2 correct" $ do
            parseScheme p2 `shouldBe` n2
        it "Parses p3 correct" $ do
            parseScheme p3 `shouldBe` n3
        it "Parses p4 correct" $ do
            parseScheme p4 `shouldBe` n4
        it "Parses p5 correct" $ do
            parseScheme p5 `shouldBe` n5
        it "Parses p6 correct" $ do
            parseScheme p6 `shouldBe` n6
        it "Parses p7 correct" $ do
            parseScheme p7 `shouldBe` n7
        it "Parses p8 correct" $ do
            parseScheme p8 `shouldBe` n8
        it "Parses p9 correct" $ do
            parseScheme p9 `shouldBe` n9
        it "Parses p10 correct" $ do
            parseScheme p10 `shouldBe` n10
        it "Parses p11 correct" $ do
            parseScheme p11 `shouldBe` n11

