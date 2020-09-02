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
p6 = [POpen, If, Identifier "p", POpen, Identifier "*", 
     Real 3.5, Integral 4, PClose, ShortQuote, POpen, Real 5, Real 9.4, PClose, PClose]
p7 = [POpen,Set, Identifier "x", POpen, Lambda, Identifier "arglist", POpen, Identifier "length"
    , Identifier "arglist", PClose,PClose, PClose]

n1 = ApplicationNode (IdentifierAtom "+") [IntegralAtom 2,IntegralAtom 5]
n2 = ApplicationNode (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") (ApplicationNode (IdentifierAtom "/") [IdentifierAtom "x",
    IdentifierAtom "y"])) [IntegralAtom 3,IntegralAtom 9,IntegralAtom 4,StringAtom "sdf"] 
n3 = ListNode [IdentifierAtom "*",IntegralAtom 2,IdentifierAtom "a"]
n4 = ListNode [IdentifierAtom "*",IntegralAtom 2,ListNode [IntegralAtom 3,
    IntegralAtom 5],IdentifierAtom "a"]
n5 = ListNode [IdentifierAtom "*",IntegralAtom 4,ApplicationNode 
    (IdentifierAtom "+") [RealAtom 3.4,IdentifierAtom "PI"],IdentifierAtom "t"]
n6 = IfNode (IdentifierAtom "p") (ApplicationNode (IdentifierAtom "*") 
    [RealAtom 3.5,IntegralAtom 4]) (ListNode [RealAtom 5.0,RealAtom 9.4])
n7 = SetNode (IdentifierAtom "x") (LambdaNode [] (IdentifierAtom "arglist") 
    (ApplicationNode (IdentifierAtom "length") [IdentifierAtom "arglist"]))

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


