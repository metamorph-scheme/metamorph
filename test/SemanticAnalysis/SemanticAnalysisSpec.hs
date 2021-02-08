module SemanticAnalysis.SemanticAnalysisSpec where
import SemanticAnalysis.SemanticAnalysis
import SemanticAnalysis.SemanticAnalysisData
import Test.Hspec

spec :: Spec
spec =
    describe "SemanticAnalysisSpec" $ do
        it "Test exists" $ do
           True `shouldBe` True
 
