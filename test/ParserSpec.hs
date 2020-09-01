import Parser
import Test.Hspec

p1 = [POpen, Identifier "+", Integral 2, Integral 5, PClose]
p2 = [POpen, POpen, Lambda,  POpen, Identifier "x", Identifier "y", Point, Identifier "z",PClose,
    POpen, Identifier "/", Identifier "x", Identifier "y", PClose, PClose, Integral 3, Integral 9, Integral 4, String "sdf", PClose]
p3 = [POpen, Quote, POpen, Identifier "*", Integral 2, Identifier "a",PClose, PClose]
p4 = [ShortQuote, POpen, Identifier "*", Integral 2, POpen, Integral 3, Integral 5, PClose ,Identifier "a",PClose]


spec :: Spec
spec =
    describe "ParserSpec" $ do
        describe "Application" $ do
            it "Applies first elemnt of List" $ do
                pending
        describe "Lambda" $ do
            it "Creates Lambda" $ do
                pending
