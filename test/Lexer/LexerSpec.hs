module Lexer.LexerSpec where

import Lexer.Lexer
import Lexer.Token

import Test.Hspec

spec :: Spec
spec = describe "Lexer.scan" $ do
  it "can discard whitespace" $ do
    scan "\n\t\r    " `shouldBe` []
  
  it "can discard single line comment" $ do
    scan "; my comment \nidentifier" `shouldBe` [Identifier "identifier"]
  
  describe "block comment" $ do
    it "can discard block comment" $ do
      scan "#| my \n block \n comment |#identifier" `shouldBe` [Identifier "identifier"]

    it "can discard block comment lazily" $ do
      scan "#| my comment |# identa #| comment 2 |#" `shouldBe` [Identifier "identa"]
  
  it "can discard whitespace and comment" $ do
    scan "; my comment \n   \tidentifier" `shouldBe` [Identifier "identifier"]
  
  it "can classify opening bracket" $ do
    scan "(" `shouldBe` [POpen]

  it "can classify closing bracket" $ do
    scan ")" `shouldBe` [PClose]

  describe "identifiers" $ do
    it "can classify identifier" $ do
      scan "identifier" `shouldBe` [Identifier "identifier"]
    
    it "can classify two identifiers" $ do
      scan "identa ident2" `shouldBe` [Identifier "identa", Identifier "ident2"]

  it "can classify lambda" $ do
    scan "lambda" `shouldBe` [Lambda]

  it "can classify set" $ do
    scan "set!" `shouldBe` [Set]

  it "can classify if" $ do
    scan "if" `shouldBe` [If]

  it "can classify dot" $ do
    scan "." `shouldBe` [Dot]