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
p8 = [ShortQuasiQuote, POpen, Identifier "x", ShortUnquote, POpen, Identifier "*",
    Real 3, Real 4, PClose, POpen, Identifier "c", Point, POpen, Integral 3, Integral 4,
     ShortUnquote,POpen, Identifier "+", Real 3, Real 9, PClose ,PClose, PClose,PClose]
p9 = [POpen, Lambda, Identifier "X", POpen, Identifier "*", Real 2, Real 3,
    PClose, POpen, Set, Identifier "X", Real 9, PClose, Identifier "X", PClose]

n1 = ApplicationNode (IdentifierAtom "+") [IntegralAtom 2,IntegralAtom 5]
n2 = ApplicationNode (LambdaNode [IdentifierAtom "x",IdentifierAtom "y"] 
    (IdentifierAtom "z") [(ApplicationNode (IdentifierAtom "/") [IdentifierAtom "x",
    IdentifierAtom "y"])]) [IntegralAtom 3,IntegralAtom 9,IntegralAtom 4,StringAtom "sdf"] 
n3 = PairNode (IdentifierAtom "*") (PairNode (IntegralAtom 2) (PairNode (IdentifierAtom "a") EmptyAtom))
n4 =PairNode (IdentifierAtom "*") (PairNode (IntegralAtom 2) (PairNode (PairNode (IntegralAtom 3) 
    (PairNode (IntegralAtom 5) EmptyAtom)) (PairNode (IdentifierAtom "a") EmptyAtom)))
n5 =PairNode (IdentifierAtom "*") (PairNode (IntegralAtom 4) (PairNode (ApplicationNode (IdentifierAtom "+") 
    [RealAtom 3.4,IdentifierAtom "PI"]) (PairNode (IdentifierAtom "t") EmptyAtom)))
n6 = IfNode (IdentifierAtom "p") (ApplicationNode (IdentifierAtom "*") [RealAtom 3.5,IntegralAtom 4]) 
    (PairNode (RealAtom 5.0) (PairNode (RealAtom 9.4) EmptyAtom))
n7 = SetNode (IdentifierAtom "x") (LambdaNode [] (IdentifierAtom "arglist") 
    [(ApplicationNode (IdentifierAtom "length") [IdentifierAtom "arglist"])])
n8 = PairNode (IdentifierAtom "x") (PairNode (ApplicationNode (IdentifierAtom "*") 
    [RealAtom 3.0,RealAtom 4.0]) (PairNode (PairNode (IdentifierAtom "c") 
    (PairNode (IntegralAtom 3) (PairNode (IntegralAtom 4) (PairNode 
    (ApplicationNode (IdentifierAtom "+") [RealAtom 3.0,RealAtom 9.0]) EmptyAtom)))) 
    EmptyAtom))
n9 = LambdaNode [] (IdentifierAtom "X") [ApplicationNode (IdentifierAtom "*") 
    [RealAtom 2.0,RealAtom 3.0],SetNode (IdentifierAtom "X") (RealAtom 9.0),IdentifierAtom "X"]

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


