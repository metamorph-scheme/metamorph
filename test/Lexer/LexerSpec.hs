module Lexer.LexerSpec where

import Lexer.Lexer
import Lexer.Token
import Data.Number
import System.IO

import Control.Exception (evaluate)
import Test.Hspec

-- (define y
--  (lambda (x) (* 2 x)))
lexer1Result = [POpen, Define, Identifier "y", POpen, Lambda, POpen, Identifier "x", PClose, POpen,
                Identifier "*", (Number . exactNumber) (Integer 2),
                Identifier "x", PClose, PClose, PClose]

spec :: Spec
spec = describe "Lexer.scan" $ do
  it "can discard whitespace" $ do
    scan "\n\t\r    " `shouldBe` []
  
  it "can discard single line comment" $ do
    scan "; my comment \nidentifier " `shouldBe` [Identifier "identifier"]
  
  describe "block comment" $ do
    it "can discard block comment" $ do
      scan "#| my \n block \n comment |# identifier " `shouldBe` [Identifier "identifier"]

    it "can discard single line block comment" $ do
      scan "#| my block comment |# identifier " `shouldBe` [Identifier "identifier"]

    it "can discard block comment lazily" $ do
      scan "#| my comment |# identa #| comment 2 |#" `shouldBe` [Identifier "identa"]
  
  it "can discard whitespace and comment" $ do
    scan "; my comment \n   \tidentifier " `shouldBe` [Identifier "identifier"]
  
  it "can classify opening bracket" $ do
    scan "(" `shouldBe` [POpen]

  it "can classify closing bracket" $ do
    scan ")" `shouldBe` [PClose]

  describe "identifiers" $ do
    it "can classify identifier" $ do
      scan "identifier " `shouldBe` [Identifier "identifier"]
    
    it "can classify two identifiers" $ do
      scan "identa ident2 " `shouldBe` [Identifier "identa", Identifier "ident2"]

    it "can classify identifier followed by bracket" $ do
      scan "identifier(a)" `shouldBe` [Identifier "identifier", POpen, Identifier "a", PClose]

    it "rejects prohibited initial character" $ do
      evaluate (scan "5dentifier") `shouldThrow` anyErrorCall

    it "can classify escaped identifier" $ do
      scan "|hello abc \t\\| \\x003a; \\a \\b |" `shouldBe` [ Identifier "hello abc \t| : \a \b " ]

  it "can classify lambda" $ do
    scan "lambda " `shouldBe` [Lambda]

  it "can classify set" $ do
    scan "set! " `shouldBe` [Set]

  it "can classify if" $ do
    scan "if " `shouldBe` [If]

  it "can classify dot" $ do
    scan ". " `shouldBe` [Dot]

  it "can classify define" $ do
    scan "define " `shouldBe` [Define]

  describe "datum comment" $ do
    it "can classify datum comment" $ do
      scan "#;" `shouldBe` [CommentDatum]
    
    it "can classify datum comment with block comment" $ do
      scan "#;#| abc |#(a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

    it "can classify datum comment with whitespace" $ do
      scan "#;  \t\n   (a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

    it "can classify datum comment with whitespace and block comment" $ do
      scan "#;#| abc |#     (a)" `shouldBe`
        [ CommentDatum
        , POpen
        , Identifier "a"
        , PClose
        ]

  describe "uinteger" $ do
    it "can classify uinteger" $ do
      scan "324 " `shouldBe` [ Number . exactNumber $ Integer 324 ]

  describe "decimal" $ do
    it "can classify decimal" $ do
      scan "3.56 " `shouldBe` [ Number . inexactNumber . Real $ InfReal 3.56 ]
  
  describe "ureal" $ do
    it "can classify ureal" $ do
      scan "4/2 " `shouldBe` [ Number . inexactNumber . Real $ InfReal 2 ]
  
  describe "real" $ do
    it "can classify real" $ do
      scan "+inf.0 " `shouldBe` [ Number . inexactNumber . Real $ PositiveInfinity ]

  describe "string" $ do
    it "can classify string" $ do
      scan "\"hello world\"" `shouldBe` [ String "hello world" ]

    it "can classify string with escaped quote" $ do
      scan "\"hello\\\" world\"" `shouldBe` [ String "hello\" world" ]

    it "can classify mnemonic escape" $ do
      scan "\"\\a\"" `shouldBe` [ String "\a" ]

  describe "real file" $ do
    it "can classify real scm file" $ do
      withFile "testdata/lexer1.scm" ReadMode (\handle -> do
        content <- hGetContents handle
        scan content `shouldBe` lexer1Result )