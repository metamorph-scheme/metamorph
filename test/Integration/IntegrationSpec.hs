module Integration.IntegrationSpec where
import Parser.Parser
import Lexer.Lexer
import System.IO
import Integration.IntegrationData
import Test.Hspec

spec :: Spec
spec = 
    describe "IntegrationSpec" $ do
        it "compiles SCIP representing tables program" $ do
            withFile "testdata/sicp-representing-tables.scm" ReadMode (\handle -> do
                content <- hGetContents handle
                let tokens = scan content 
                parseScheme tokens `shouldBe` scipRepresentingTablesAst )
        it "compiles program composed from multiple toplevel constructs" $ do
            withFile "testdata/multexpr.scm" ReadMode (\handle -> do
                content <- hGetContents handle
                let tokens = scan content 
                parseScheme tokens `shouldBe` multExprAst)