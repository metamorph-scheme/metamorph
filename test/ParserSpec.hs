module ParserSpec where
import Parser
import Control.Monad.State.Lazy
import Test.Hspec

p1 = [POpen, Identifier "+", Integral 2, Integral 5, PClose]
p2 = [POpen, POpen, Lambda,  POpen, Identifier "x", Identifier "y", Point, Identifier "z",PClose,
    POpen, Identifier "/", Identifier "x", Identifier "y", PClose, PClose, Integral 3, Integral 9, Integral 4, String "sdf", PClose]
p3 = [POpen, Quote, POpen, Identifier "*", Integral 2, Identifier "a",PClose, PClose]
p4 = [ShortQuote, POpen, Identifier "*", Integral 2, POpen, Integral 3, Integral 5, PClose ,Identifier "a",PClose]
p5 = [POpen, QuasiQuote, POpen, Identifier "*", Integral 4, POpen, Unquote, POpen, Identifier "+",
    Real 3.4, Identifier "PI", PClose,PClose, Identifier "t", PClose, PClose]


spec :: Spec
spec =
    describe "ParserSpec" $ do
        it "Parses p1 to end" $ do
            runState parse p1 `shouldSatisfy` (\(x, y) -> null y)
        it "Parses p2 to end" $ do
            runState parse p2 `shouldSatisfy` (\(x, y) -> null y)
        it "Parses p3 to end" $ do
            runState parse p3 `shouldSatisfy` (\(x, y) -> null y)
        it "Parses p4 to end" $ do
            runState parse p4 `shouldSatisfy` (\(x, y) -> null y)
        it "Parses p5 to end" $ do
            runState parse p5 `shouldSatisfy` (\(x, y) -> null y)


