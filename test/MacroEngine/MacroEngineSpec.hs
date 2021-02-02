module MacroEngine.MacroEngineSpec where

import MacroEngine.MacroEngine
import Parser.MetaNode
import Common.Number
import qualified Data.Map as Map

import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = 
  let
    ellipsis = "..."
  in
  describe "MacroEngine" $ do
    describe "MacroEngine.match" $ do
      it "matches underscore" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "_")} (IdentifierAtom "blah") `shouldBe` Just emptyTree
      it "matches non-literal" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "pattern")} (IdentifierAtom "blah") `shouldBe` Just (singletonTree (IdentifierAtom "pattern") (IdentifierAtom "blah"))
      it "matches non-literal String" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "pattern")} (StringAtom "blah") `shouldBe` Just (singletonTree (IdentifierAtom "pattern") (StringAtom "blah"))
      it "does not match literal with different binding" $ do
        match Pattern{literals=["lit"], ellipsis=ellipsis, patternNode=(IdentifierAtom "lit")} (IdentifierAtom "blah") `shouldBe` Nothing
      it "matches literal with same binding" $ do
        match Pattern{literals=["lit"], ellipsis=ellipsis, patternNode=(IdentifierAtom "lit")} (IdentifierAtom "lit") `shouldBe` Just (singletonTree (IdentifierAtom "lit") (IdentifierAtom "lit"))
      it "matches number constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(NumberAtom (Exact (Integer 3)))} (NumberAtom (Exact (Integer 3))) `shouldBe` Just emptyTree
      it "does not match number constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(NumberAtom (Exact (Integer 3)))} (NumberAtom (Exact (Integer 4))) `shouldBe` Nothing
      it "matches string constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(StringAtom "mystr")} (StringAtom "mystr") `shouldBe` Just emptyTree
      it "does not match string constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(StringAtom "mystr")} (StringAtom "mystrde") `shouldBe` Nothing
      it "matches bool constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(BoolAtom True)} (BoolAtom True) `shouldBe` Just emptyTree
      it "does not match bool constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(BoolAtom False)} (BoolAtom True) `shouldBe` Nothing
      it "matches char constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(CharAtom 'd')} (CharAtom 'd') `shouldBe` Just emptyTree
      it "does not match chat constant" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(CharAtom 'e')} (CharAtom 'd') `shouldBe` Nothing

    describe "MacroEngine.matchList" $ do
      describe "regular list" $ do
        it "matches list of equal lenght" $ do
          let
            patterns = PairNode (IdentifierAtom "hansi") (PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "hansi") (IdentifierAtom "node"), Value (IdentifierAtom "a") (IdentifierAtom "anothernode"), Value (IdentifierAtom "b") (IdentifierAtom "b")]
        it "does not match longer param list" $ do
          let
            patterns = PairNode (IdentifierAtom "hansi") (PairNode (IdentifierAtom "a") EmptyAtom)
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
        it "does not match longer pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "hansi") (PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
      describe "ellipsis list" $ do
        it "matches ellipsis [multiple][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Ellipsis (IdentifierAtom "a") [EllipsisValue (IdentifierAtom "node"), EllipsisValue (IdentifierAtom "anothernode"), EllipsisValue (IdentifierAtom "b"), EllipsisValue (IdentifierAtom "la")], Value (IdentifierAtom "b") (IdentifierAtom "blah")] 
        it "matches ellipsis [one][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Ellipsis (IdentifierAtom "a") [EllipsisValue (IdentifierAtom "node")], Value (IdentifierAtom "b") (IdentifierAtom "la")]
        it "matches ellipsis [zero][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Ellipsis (IdentifierAtom "a") [], Value (IdentifierAtom "b") (IdentifierAtom "la")]
        it "matches ellipsis [multiple][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), Ellipsis (IdentifierAtom "b") [EllipsisValue (IdentifierAtom "anothernode"), EllipsisValue (IdentifierAtom "b"), EllipsisValue (IdentifierAtom "la")], Value (IdentifierAtom "c") (IdentifierAtom "blah")] 
        it "matches ellipsis [one][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), Ellipsis (IdentifierAtom "b") [EllipsisValue (IdentifierAtom "la")], Value (IdentifierAtom "c") (IdentifierAtom "blah")] 
        it "matches ellipsis [zero][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "la"), Ellipsis (IdentifierAtom "b") [], Value (IdentifierAtom "c") (IdentifierAtom "blah")]
        it "matches ellipsis [multiple][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), Ellipsis (IdentifierAtom "b") [EllipsisValue (IdentifierAtom "anothernode"), EllipsisValue (IdentifierAtom "b"), EllipsisValue (IdentifierAtom "la"), EllipsisValue (IdentifierAtom "blah")]] 
        it "matches ellipsis [one][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), Ellipsis (IdentifierAtom "b") [EllipsisValue (IdentifierAtom "la")]]
        it "matches ellipsis [zero][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "la"), Ellipsis (IdentifierAtom "b") []]
        it "does not match too short params with ellipsis" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "c") (PairNode (IdentifierAtom "...") EmptyAtom)))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
        it "matches subpatterns in ellipsis" $ do
          let
            patterns = PairNode (PairNode (IdentifierAtom "name") (PairNode (IdentifierAtom "value") EmptyAtom)) (PairNode (IdentifierAtom "...") EmptyAtom)
            params = PairNode (PairNode (IdentifierAtom "one") (PairNode (NumberAtom (Exact (Integer 3))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "two") (PairNode (NumberAtom (Exact (Integer 4))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "three") (PairNode (NumberAtom (Exact (Integer 2))) EmptyAtom)) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Ellipsis (PairNode (IdentifierAtom "name") (PairNode (IdentifierAtom "value") EmptyAtom)) [EllipsisSubPattern [Value (IdentifierAtom "name") (IdentifierAtom "one"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 3)))],EllipsisSubPattern [Value (IdentifierAtom "name") (IdentifierAtom "two"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 4)))],EllipsisSubPattern [Value (IdentifierAtom "name") (IdentifierAtom "three"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 2)))]]]
      describe "sublist" $ do
        it "matches sublists" $ do
          let
            patterns = PairNode (IdentifierAtom "ta") (PairNode (PairNode (IdentifierAtom "tb") (PairNode (IdentifierAtom "tc") EmptyAtom)) EmptyAtom)
            params = PairNode (IdentifierAtom "ta") (PairNode (PairNode (IdentifierAtom "tb") (PairNode (IdentifierAtom "tc") EmptyAtom)) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "ta") (IdentifierAtom "ta"), Value (IdentifierAtom "tb") (IdentifierAtom "tb"), Value (IdentifierAtom "tc") (IdentifierAtom "tc")]
      -- it "matches sublists with ellipsis" $ do
      --   let
      --     patterns = PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "...") (PairNode (PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "tb") EmptyAtom)) EmptyAtom))
      --     params = PairNode (IdentifierAtom "ta") (PairNode (PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "tb") EmptyAtom)) EmptyAtom)
      --   matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` True
      -- it "does not match wrong lenght sublists with ellipsis" $ do
      --   let
      --     patterns = PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "...") (PairNode (PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "tb") EmptyAtom)) EmptyAtom))
      --     params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anode") (PairNode (IdentifierAtom "twonode") (PairNode (PairNode (IdentifierAtom "ta") (PairNode (IdentifierAtom "tb") (PairNode (IdentifierAtom "a") EmptyAtom))) EmptyAtom)))
      --   matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
      describe "error cases" $ do
        it "fails when ellipsis is first element of pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
        it "fails when multiple ellipsis are in a pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
        -- can't be tested because error call is inside nubBy function
        -- it "fails when a pattern variable is duplicated" $ do
        --   let
        --     patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") EmptyAtom))
        --     params = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") EmptyAtom))
        --   evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
    describe "MacroEngine.matchApplication" $ do
      it "ignores first part of pattern" $ do
        let
          patterns = PairNode (IdentifierAtom "id") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "c") EmptyAtom))
          params = ApplicationNode (IdentifierAtom "lala") [IdentifierAtom "pb", IdentifierAtom "pc"]
        matchApplication Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "b") (IdentifierAtom "pb"), Value (IdentifierAtom "c") (IdentifierAtom "pc")]
      it "fails when params are no application node" $ do
        let
          patterns = PairNode (IdentifierAtom "id") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "c") EmptyAtom))
          params = PairNode (IdentifierAtom "lala") (PairNode (IdentifierAtom "pb") (PairNode (IdentifierAtom "pc") EmptyAtom))
        evaluate (matchApplication Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
    
    describe "MacroEngine.transform" $ do
      it "transform bindings one first level" $ do
        transform [Value (IdentifierAtom "ident") (NumberAtom (Exact (Integer 3)))] (IdentifierAtom "ident") `shouldBe` (NumberAtom (Exact (Integer 3)))


    describe "MacroEngine.applySyntaxRules" $ do
      it "transforms single level macro" $ do
        let
          -- (syntax-rules () ((hansi (a b)) (b a)))
          syntaxRules = ApplicationNode
            (IdentifierAtom "syntax-rules")
            [
              EmptyAtom,
              ApplicationNode
                (ApplicationNode
                  (IdentifierAtom "hansi")
                  [
                    ApplicationNode
                      (IdentifierAtom "a")
                      [IdentifierAtom "b"]
                  ]
                )
                [
                  ApplicationNode
                    (IdentifierAtom "b")
                    [IdentifierAtom "a"]
                ]
            ]
          -- (flip (1 2))
          application = ApplicationNode (IdentifierAtom "flip") [ApplicationNode (NumberAtom (Exact (Integer 1))) [NumberAtom (Exact (Integer 2))]]
        -- shouldbe (2 1)
        applySyntaxRules syntaxRules application `shouldBe` ApplicationNode (NumberAtom (Exact (Integer 2))) [NumberAtom (Exact (Integer 1))]
          