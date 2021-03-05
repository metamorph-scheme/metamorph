module MacroEngine.MacroEngineSpec where

import MacroEngine.MacroEngine
import MacroEngine.TemplateMetaNode
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
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "_" 0)} (IdentifierAtom "blah" 0) `shouldBe` Just emptyTree
      it "matches non-literal" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "pattern" 0)} (IdentifierAtom "blah" 0) `shouldBe` Just (singletonTree (IdentifierAtom "pattern" 0) (IdentifierAtom "blah" 0))
      it "matches non-literal String" $ do
        match Pattern{literals=[], ellipsis=ellipsis, patternNode=(IdentifierAtom "pattern" 0)} (StringAtom "blah") `shouldBe` Just (singletonTree (IdentifierAtom "pattern" 0) (StringAtom "blah"))
      it "does not match literal with different binding" $ do
        match Pattern{literals=["lit"], ellipsis=ellipsis, patternNode=(IdentifierAtom "lit" 0)} (IdentifierAtom "blah" 0) `shouldBe` Nothing
      it "matches literal with same binding" $ do
        match Pattern{literals=["lit"], ellipsis=ellipsis, patternNode=(IdentifierAtom "lit" 0)} (IdentifierAtom "lit" 0) `shouldBe` Just (singletonTree (IdentifierAtom "lit" 0) (IdentifierAtom "lit" 0))
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
            patterns = PairNode (IdentifierAtom "hansi" 0) (PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "hansi" 0) (IdentifierAtom "node" 0), Value (IdentifierAtom "a" 0) (IdentifierAtom "anothernode" 0), Value (IdentifierAtom "b" 0) (IdentifierAtom "b" 0)]
        it "does not match longer param list" $ do
          let
            patterns = PairNode (IdentifierAtom "hansi" 0) (PairNode (IdentifierAtom "a" 0) EmptyAtom)
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
        it "does not match longer pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "hansi" 0) (PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
      describe "ellipsis list" $ do
        it "matches ellipsis [multiple][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "la" 0) (PairNode (IdentifierAtom "blah" 0) EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a" 0) [ (IdentifierAtom "node" 0),  (IdentifierAtom "anothernode" 0),  (IdentifierAtom "b" 0),  (IdentifierAtom "la" 0)], Value (IdentifierAtom "b" 0) (IdentifierAtom "blah" 0)] 
        it "matches ellipsis [one][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "la" 0) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a" 0) [ (IdentifierAtom "node" 0)], Value (IdentifierAtom "b" 0) (IdentifierAtom "la" 0)]
        it "matches ellipsis [zero][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "la" 0) EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a" 0) [], Value (IdentifierAtom "b" 0) (IdentifierAtom "la" 0)]
        it "matches ellipsis [multiple][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "c" 0) EmptyAtom)))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "la" 0) (PairNode (IdentifierAtom "blah" 0) EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "node" 0), IdentifierEllipsis (IdentifierAtom "b" 0) [ (IdentifierAtom "anothernode" 0),  (IdentifierAtom "b" 0),  (IdentifierAtom "la" 0)], Value (IdentifierAtom "c" 0) (IdentifierAtom "blah" 0)] 
        it "matches ellipsis [one][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "c" 0) EmptyAtom)))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "la" 0) (PairNode (IdentifierAtom "blah" 0) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "node" 0), IdentifierEllipsis (IdentifierAtom "b" 0) [ (IdentifierAtom "la" 0)], Value (IdentifierAtom "c" 0) (IdentifierAtom "blah" 0)] 
        it "matches ellipsis [zero][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "c" 0) EmptyAtom)))
            params = PairNode (IdentifierAtom "la" 0) (PairNode (IdentifierAtom "blah" 0) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "la" 0), IdentifierEllipsis (IdentifierAtom "b" 0) [], Value (IdentifierAtom "c" 0) (IdentifierAtom "blah" 0)]
        it "matches ellipsis [multiple][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anothernode" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "la" 0) (PairNode (IdentifierAtom "blah" 0) EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "node" 0), IdentifierEllipsis (IdentifierAtom "b" 0) [ (IdentifierAtom "anothernode" 0),  (IdentifierAtom "b" 0),  (IdentifierAtom "la" 0),  (IdentifierAtom "blah" 0)]] 
        it "matches ellipsis [one][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) EmptyAtom))
            params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "la" 0) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "node" 0), IdentifierEllipsis (IdentifierAtom "b" 0) [ (IdentifierAtom "la" 0)]]
        it "matches ellipsis [zero][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "..." 0) EmptyAtom))
            params = PairNode (IdentifierAtom "la" 0) EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a" 0) (IdentifierAtom "la" 0), IdentifierEllipsis (IdentifierAtom "b" 0) []]
        it "does not match too short params with ellipsis" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "c" 0) (PairNode (IdentifierAtom "..." 0) EmptyAtom)))
            params = PairNode (IdentifierAtom "la" 0) EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
        it "matches subpatterns in ellipsis" $ do
          let
            patterns = PairNode (PairNode (IdentifierAtom "name" 0) (PairNode (IdentifierAtom "value" 0) EmptyAtom)) (PairNode (IdentifierAtom "..." 0) EmptyAtom)
            params = PairNode (PairNode (IdentifierAtom "one" 0) (PairNode (NumberAtom (Exact (Integer 3))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "two" 0) (PairNode (NumberAtom (Exact (Integer 4))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "three" 0) (PairNode (NumberAtom (Exact (Integer 2))) EmptyAtom)) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [SubPatternEllipsis (PairNode (IdentifierAtom "name" 0) (PairNode (IdentifierAtom "value" 0) EmptyAtom)) [[Value (IdentifierAtom "name" 0) (IdentifierAtom "one" 0),Value (IdentifierAtom "value" 0) (NumberAtom (Exact (Integer 3)))],[Value (IdentifierAtom "name" 0) (IdentifierAtom "two" 0),Value (IdentifierAtom "value" 0) (NumberAtom (Exact (Integer 4)))],[Value (IdentifierAtom "name" 0) (IdentifierAtom "three" 0),Value (IdentifierAtom "value" 0) (NumberAtom (Exact (Integer 2)))]]]
      describe "sublist" $ do
        it "matches sublists" $ do
          let
            patterns = PairNode (IdentifierAtom "ta" 0) (PairNode (PairNode (IdentifierAtom "tb" 0) (PairNode (IdentifierAtom "tc" 0) EmptyAtom)) EmptyAtom)
            params = PairNode (IdentifierAtom "ta" 0) (PairNode (PairNode (IdentifierAtom "tb" 0) (PairNode (IdentifierAtom "tc" 0) EmptyAtom)) EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "ta" 0) (IdentifierAtom "ta" 0), Value (IdentifierAtom "tb" 0) (IdentifierAtom "tb" 0), Value (IdentifierAtom "tc" 0) (IdentifierAtom "tc" 0)]
      -- it "matches sublists with ellipsis" $ do
      --   let
      --     patterns = PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "tb" 0) EmptyAtom)) EmptyAtom))
      --     params = PairNode (IdentifierAtom "ta" 0) (PairNode (PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "tb" 0) EmptyAtom)) EmptyAtom)
      --   matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` True
      -- it "does not match wrong lenght sublists with ellipsis" $ do
      --   let
      --     patterns = PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "tb" 0) EmptyAtom)) EmptyAtom))
      --     params = PairNode (IdentifierAtom "node" 0) (PairNode (IdentifierAtom "anode" 0) (PairNode (IdentifierAtom "twonode" 0) (PairNode (PairNode (IdentifierAtom "ta" 0) (PairNode (IdentifierAtom "tb" 0) (PairNode (IdentifierAtom "a" 0) EmptyAtom))) EmptyAtom)))
      --   matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
      describe "error cases" $ do
        it "fails when ellipsis is first element of pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
            params = PairNode (IdentifierAtom "la" 0) EmptyAtom
          evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
        it "fails when multiple ellipsis are in a pattern list" $ do
          let
            patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "..." 0) (PairNode (IdentifierAtom "..." 0) EmptyAtom))
            params = PairNode (IdentifierAtom "la" 0) EmptyAtom
          evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
        -- can't be tested because error call is inside nubBy function
        -- it "fails when a pattern variable is duplicated" $ do
        --   let
        --     patterns = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
        --     params = PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "a" 0) (PairNode (IdentifierAtom "b" 0) EmptyAtom))
        --   evaluate (matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
    describe "MacroEngine.matchApplication" $ do
      it "ignores first part of pattern" $ do
        let
          patterns = PairNode (IdentifierAtom "id" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "c" 0) EmptyAtom))
          params = ApplicationNode (IdentifierAtom "lala" 0) [IdentifierAtom "pb" 0, IdentifierAtom "pc" 0]
        matchApplication Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "b" 0) (IdentifierAtom "pb" 0), Value (IdentifierAtom "c" 0) (IdentifierAtom "pc" 0)]
      it "fails when params are no application node" $ do
        let
          patterns = PairNode (IdentifierAtom "id" 0) (PairNode (IdentifierAtom "b" 0) (PairNode (IdentifierAtom "c" 0) EmptyAtom))
          params = PairNode (IdentifierAtom "lala" 0) (PairNode (IdentifierAtom "pb" 0) (PairNode (IdentifierAtom "pc" 0) EmptyAtom))
        evaluate (matchApplication Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params) `shouldThrow` anyErrorCall
    
    describe "MacroEngine.transform" $ do
      it "transform bindings on root level" $ do
        transform [Value (IdentifierAtom "ident" 0) (NumberAtom (Exact (Integer 3)))] (TemplateIdentifierAtom [0] "ident" 0) `shouldBe` (TemplateAtom (NumberAtom (Exact (Integer 3))))

      it "transform subtemplate ellipsis" $ do
        -- pattern (hansi (name value) ... a) / application (hansi (a b) (c d) matcha)
        let bindingTree = [SubPatternEllipsis (ApplicationNode (IdentifierAtom "name" 0) [IdentifierAtom "value" 0]) [[Value (IdentifierAtom "name" 0) (IdentifierAtom "a" 0),Value (IdentifierAtom "value" 0) (IdentifierAtom "b" 0)],[Value (IdentifierAtom "name" 0) (IdentifierAtom "c" 0),Value (IdentifierAtom "value" 0) (IdentifierAtom "d" 0)]],Value (IdentifierAtom "a" 0) (IdentifierAtom "matcha" 0)]
        -- template (list (value name b a) ...)
        let template = TemplateListNode [TemplateIdentifierAtom [1] "list" 0,TemplateEllipsisNode 1 [0] (TemplateListNode [TemplateIdentifierAtom [0,3] "value" 0,TemplateIdentifierAtom [0,2] "name" 0,TemplateIdentifierAtom [0,1] "b" 0,TemplateIdentifierAtom [0,0] "a" 0])]
        -- result (list (b a b matcha) (d c b matcha))
        transform bindingTree template `shouldBe` 
          TemplateListNode [
            TemplateIdentifierAtom [1] "list" 0,
            TemplateListNode [
              TemplateAtom (IdentifierAtom "b" 1),
              TemplateAtom (IdentifierAtom "a" 1),
              TemplateIdentifierAtom [0,1] "b" 0,
              TemplateAtom (IdentifierAtom "matcha" 1)],
            TemplateListNode [
              TemplateAtom (IdentifierAtom "d" 1),
              TemplateAtom (IdentifierAtom "c" 1),
              TemplateIdentifierAtom [0,1] "b" 0,
              TemplateAtom (IdentifierAtom "matcha" 1)]]

      -- it "transform subtemplate ellipsis" $ do
      --   -- pattern (hansi ((name ...) (value ...)) ...) / application (hansi ((1 2 3) (4 5 6)) ((3 2 1) (6 5 4)))
      --   let bindingTree = [SubPatternEllipsis (ApplicationNode (ApplicationNode (IdentifierAtom "name" 0) [IdentifierAtom "..." 0]) [ApplicationNode (IdentifierAtom "value" 0) [IdentifierAtom "..." 0]]) [[IdentifierEllipsis (IdentifierAtom "name" 0) [NumberAtom (Exact (Integer 1)),NumberAtom (Exact (Integer 2)),NumberAtom (Exact (Integer 3))],IdentifierEllipsis (IdentifierAtom "value" 0) [NumberAtom (Exact (Integer 4)),NumberAtom (Exact (Integer 5)),NumberAtom (Exact (Integer 6))]],[IdentifierEllipsis (IdentifierAtom "name" 0) [NumberAtom (Exact (Integer 3)),NumberAtom (Exact (Integer 2)),NumberAtom (Exact (Integer 1))],IdentifierEllipsis (IdentifierAtom "value" 0) [NumberAtom (Exact (Integer 6)),NumberAtom (Exact (Integer 5)),NumberAtom (Exact (Integer 4))]]]]
      --   -- template (list (list name ...) ...)
      --   let template = TemplateListNode [TemplateIdentifierAtom [1] "list" 0,TemplateEllipsisNode 1 [0] (TemplateListNode [TemplateIdentifierAtom [0,1] "list" 0,TemplateEllipsisNode 1 [0,0] (TemplateIdentifierAtom [0,0] "name" 0)])]
      --   -- result (list (list 1 2 3) (list 3 2 1))
      --   transform bindingTree template `shouldBe` 
      --     TemplateListNode [
      --       TemplateIdentifierAtom [1] "list" 0,
      --       TemplateListNode [
      --         TemplateIdentifierAtom [0,1] "list" 0,
      --         TemplateAtom (NumberAtom (Exact (Integer 1))),
      --         TemplateAtom (NumberAtom (Exact (Integer 2))),
      --         TemplateAtom (NumberAtom (Exact (Integer 3)))
      --       ],
      --       TemplateListNode [
      --         TemplateIdentifierAtom [0,1] "list" 0,
      --         TemplateAtom (NumberAtom (Exact (Integer 3))),
      --         TemplateAtom (NumberAtom (Exact (Integer 2))),
      --         TemplateAtom (NumberAtom (Exact (Integer 1)))
      --       ]
      --     ]

      -- it "transform subtemplate ellipsis" $ do
      --   -- pattern (hansi (name ...) ...) / application (hansi (1 2 3) (4 5 6))
      --   let bindingTree = [SubPatternEllipsis (ApplicationNode (IdentifierAtom "name" 0) [IdentifierAtom "..." 0]) [[IdentifierEllipsis (IdentifierAtom "name" 0) [NumberAtom (Exact (Integer 1)),NumberAtom (Exact (Integer 2)),NumberAtom (Exact (Integer 3))]],[IdentifierEllipsis (IdentifierAtom "name" 0) [NumberAtom (Exact (Integer 4)),NumberAtom (Exact (Integer 5)),NumberAtom (Exact (Integer 6))]]]]
      --   -- template (list (b (a name) ...) ...)
      --   let template = TemplateListNode [TemplateIdentifierAtom [1] "list" 0,TemplateEllipsisNode 1 [0] (TemplateListNode [TemplateIdentifierAtom [0,1] "b" 0,TemplateEllipsisNode 1 [0,0] (TemplateListNode [TemplateIdentifierAtom [0,0,1] "a" 0,TemplateIdentifierAtom [0,0,0] "name" 0])])]
      --   -- result (list (b (a 1) (a 2) (a 3)) (b (a 4) (a 5) (a 6)))
      --   transform bindingTree template `shouldBe` 
      --     TemplateListNode [
      --       TemplateIdentifierAtom [1] "list" 0,
      --       TemplateListNode [
      --         TemplateIdentifierAtom [0,1] "b" 0,
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 1)))
      --         ],
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 2)))
      --         ],
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 3)))
      --         ]
      --       ],
      --       TemplateListNode [
      --         TemplateIdentifierAtom [0,1] "b" 0,
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 4)))
      --         ],
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 5)))
      --         ],
      --         TemplateListNode [
      --           TemplateIdentifierAtom [0,0,1] "a" 0,
      --           TemplateAtom (NumberAtom (Exact (Integer 6)))
      --         ]
      --       ]
      --     ]

    describe "MacroEngine.applySyntaxRules" $ do
      it "transforms single level macro" $ do
        let
          -- (syntax-rules () ((hansi (a b)) (b a)))
          syntaxRules = ApplicationNode
            (IdentifierAtom "syntax-rules" 0)
            [
              EmptyAtom,
              ApplicationNode
                (ApplicationNode
                  (IdentifierAtom "hansi" 0)
                  [
                    ApplicationNode
                      (IdentifierAtom "a" 0)
                      [IdentifierAtom "b" 0]
                  ]
                )
                [
                  ApplicationNode
                    (IdentifierAtom "b" 0)
                    [IdentifierAtom "a" 0]
                ]
            ]
          -- (flip (1 2))
          application = ApplicationNode (IdentifierAtom "flip" 0) [ApplicationNode (NumberAtom (Exact (Integer 1))) [NumberAtom (Exact (Integer 2))]]
        -- shouldbe (2 1)
        applySyntaxRules syntaxRules application `shouldBe` ApplicationNode (NumberAtom (Exact (Integer 2))) [NumberAtom (Exact (Integer 1))]

    describe "MacroEngine.analyseTemplateList" $ do
      it "resolves single ellipsis" $ do
        analyseTemplateList ellipsis [] [IdentifierAtom "a" 0, IdentifierAtom "..." 0, IdentifierAtom "b" 0] `shouldBe` [TemplateEllipsisNode 1 [1] (TemplateIdentifierAtom [1,0] "a" 0), TemplateIdentifierAtom [0] "b" 0]

      it "resolves double ellipsis" $ do
        analyseTemplateList ellipsis [] [IdentifierAtom "a" 0, IdentifierAtom "..." 0, IdentifierAtom "..." 0, IdentifierAtom "b" 0] `shouldBe` [TemplateEllipsisNode 2 [1] (TemplateIdentifierAtom [1,0] "a" 0), TemplateIdentifierAtom [0] "b" 0]

    describe "MacroEngine.lookup" $ do
      it "looks up simple ellipse correctly" $ do
        bindingLookup 1 "name" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "name" 0) [IdentifierAtom "value" 0])
          [
            [Value (IdentifierAtom "name" 0) (StringAtom "rap"), Value (IdentifierAtom "value" 0) (StringAtom "gru")],
            [Value (IdentifierAtom "name" 0) (StringAtom "gru"), Value (IdentifierAtom "value" 0) (StringAtom "rap")]
          ]]
        `shouldBe`
        Just [StringAtom "rap", StringAtom "gru"]
      it "looks up double ellipse correctly" $ do
        bindingLookup 2 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e" 0) [IdentifierAtom "..." 0])
          [
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "c", StringAtom "d"]]
          ]]
        `shouldBe`
        Just [StringAtom "a", StringAtom "b", StringAtom "c", StringAtom "d"]
      it "throws error when template has not enough ellipses" $ do
        evaluate (bindingLookup 1 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e" 0) [IdentifierAtom "..." 0])
          [
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "c", StringAtom "d"]]
          ]])
        `shouldThrow`
        anyErrorCall
      it "throws error when template has too many ellipses" $ do
        evaluate (bindingLookup 3 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e" 0) [IdentifierAtom "..." 0])
          [
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "c", StringAtom "d"]]
          ]])
        `shouldThrow`
        anyErrorCall
      it "returns Nothing if identifier is a free reference" $ do
        bindingLookup 2 "free" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e" 0) [IdentifierAtom "..." 0])
          [
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e" 0) [StringAtom "c", StringAtom "d"]]
          ]]
        `shouldBe`
        Nothing
    
    describe "MacroEngine.makroengineBase" $ do
      it "resolves let" $ do
        let application = ApplicationNode (IdentifierAtom "let" 0) [ApplicationNode (ApplicationNode (IdentifierAtom "a" 0) [StringAtom "abc"]) [],IdentifierAtom "a" 0]
        makroengineBase "let" application `shouldBe` ApplicationNode (LambdaNode [IdentifierAtom "a" 1] (IdentifierAtom "" 0) [IdentifierAtom "a" 1]) [StringAtom "abc"]
    
    describe "MacroEngine.makroengineLetIdentifiers" $ do
      it "returns identifiers bound by let-syntax" $ do
        let bindings = ApplicationNode (ApplicationNode (IdentifierAtom "hansi" 0) [ApplicationNode (IdentifierAtom "syntax-rules" 0) [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "_" 0) [IdentifierAtom "x" 0,IdentifierAtom "y" 0]) [LetSyntaxNode (ApplicationNode (ApplicationNode (IdentifierAtom "syn" 0) [ApplicationNode (IdentifierAtom "syntax-rules" 0) [EmptyAtom,ApplicationNode (ApplicationNode (IdentifierAtom "_" 0) [IdentifierAtom "a" 0]) [ApplicationNode (IdentifierAtom "odd?" 0) [IdentifierAtom "a" 0]]]]) []) [ApplicationNode (IdentifierAtom "y" 0) [ApplicationNode (IdentifierAtom "syn" 0) [IdentifierAtom "x" 0]]]]]]) []
        makroengineLetIdentifiers bindings `shouldBe` [IdentifierAtom "hansi" 0]