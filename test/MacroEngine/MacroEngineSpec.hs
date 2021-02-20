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
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a") [ (IdentifierAtom "node"),  (IdentifierAtom "anothernode"),  (IdentifierAtom "b"),  (IdentifierAtom "la")], Value (IdentifierAtom "b") (IdentifierAtom "blah")] 
        it "matches ellipsis [one][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a") [ (IdentifierAtom "node")], Value (IdentifierAtom "b") (IdentifierAtom "la")]
        it "matches ellipsis [zero][beginning]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "b") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [IdentifierEllipsis (IdentifierAtom "a") [], Value (IdentifierAtom "b") (IdentifierAtom "la")]
        it "matches ellipsis [multiple][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), IdentifierEllipsis (IdentifierAtom "b") [ (IdentifierAtom "anothernode"),  (IdentifierAtom "b"),  (IdentifierAtom "la")], Value (IdentifierAtom "c") (IdentifierAtom "blah")] 
        it "matches ellipsis [one][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), IdentifierEllipsis (IdentifierAtom "b") [ (IdentifierAtom "la")], Value (IdentifierAtom "c") (IdentifierAtom "blah")] 
        it "matches ellipsis [zero][middle]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") (PairNode (IdentifierAtom "c") EmptyAtom)))
            params = PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "la"), IdentifierEllipsis (IdentifierAtom "b") [], Value (IdentifierAtom "c") (IdentifierAtom "blah")]
        it "matches ellipsis [multiple][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "anothernode") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "la") (PairNode (IdentifierAtom "blah") EmptyAtom))))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), IdentifierEllipsis (IdentifierAtom "b") [ (IdentifierAtom "anothernode"),  (IdentifierAtom "b"),  (IdentifierAtom "la"),  (IdentifierAtom "blah")]] 
        it "matches ellipsis [one][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "node") (PairNode (IdentifierAtom "la") EmptyAtom)
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "node"), IdentifierEllipsis (IdentifierAtom "b") [ (IdentifierAtom "la")]]
        it "matches ellipsis [zero][end]" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "...") EmptyAtom))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [Value (IdentifierAtom "a") (IdentifierAtom "la"), IdentifierEllipsis (IdentifierAtom "b") []]
        it "does not match too short params with ellipsis" $ do
          let
            patterns = PairNode (IdentifierAtom "a") (PairNode (IdentifierAtom "b") (PairNode (IdentifierAtom "c") (PairNode (IdentifierAtom "...") EmptyAtom)))
            params = PairNode (IdentifierAtom "la") EmptyAtom
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Nothing
        it "matches subpatterns in ellipsis" $ do
          let
            patterns = PairNode (PairNode (IdentifierAtom "name") (PairNode (IdentifierAtom "value") EmptyAtom)) (PairNode (IdentifierAtom "...") EmptyAtom)
            params = PairNode (PairNode (IdentifierAtom "one") (PairNode (NumberAtom (Exact (Integer 3))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "two") (PairNode (NumberAtom (Exact (Integer 4))) EmptyAtom)) (PairNode (PairNode (IdentifierAtom "three") (PairNode (NumberAtom (Exact (Integer 2))) EmptyAtom)) EmptyAtom))
          matchList Pattern{literals=[], ellipsis=ellipsis, patternNode=patterns} params `shouldBe` Just [SubPatternEllipsis (PairNode (IdentifierAtom "name") (PairNode (IdentifierAtom "value") EmptyAtom)) [[Value (IdentifierAtom "name") (IdentifierAtom "one"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 3)))],[Value (IdentifierAtom "name") (IdentifierAtom "two"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 4)))],[Value (IdentifierAtom "name") (IdentifierAtom "three"),Value (IdentifierAtom "value") (NumberAtom (Exact (Integer 2)))]]]
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
      it "transform bindings on root level" $ do
        transform [Value (IdentifierAtom "ident") (NumberAtom (Exact (Integer 3)))] (TemplateIdentifierAtom [0] "ident") `shouldBe` (TemplateAtom (NumberAtom (Exact (Integer 3))))

      it "transform subtemplate ellipsis" $ do
        -- pattern (hansi (name value) ... a) / application (hansi (a b) (c d) matcha)
        let bindingTree = [SubPatternEllipsis (ApplicationNode (IdentifierAtom "name") [IdentifierAtom "value"]) [[Value (IdentifierAtom "name") (IdentifierAtom "a"),Value (IdentifierAtom "value") (IdentifierAtom "b")],[Value (IdentifierAtom "name") (IdentifierAtom "c"),Value (IdentifierAtom "value") (IdentifierAtom "d")]],Value (IdentifierAtom "a") (IdentifierAtom "matcha")]
        -- template (list (value name b a) ...)
        let template = TemplateListNode [TemplateIdentifierAtom [1] "list",TemplateEllipsisNode 1 [0] (TemplateListNode [TemplateIdentifierAtom [0,3] "value",TemplateIdentifierAtom [0,2] "name",TemplateIdentifierAtom [0,1] "b",TemplateIdentifierAtom [0,0] "a"])]
        -- result (list (b a b matcha) (d c b matcha))
        transform bindingTree template `shouldBe` 
          TemplateListNode [TemplateIdentifierAtom [1] "list",
            TemplateListNode [
              TemplateAtom (IdentifierAtom "b"),
              TemplateAtom (IdentifierAtom "a"),
              TemplateIdentifierAtom [0,1] "b",
              TemplateAtom (IdentifierAtom "matcha")],
            TemplateListNode [
              TemplateAtom (IdentifierAtom "d"),
              TemplateAtom (IdentifierAtom "c"),
              TemplateIdentifierAtom [0,1] "b",
              TemplateAtom (IdentifierAtom "matcha")]]

      it "transform subtemplate ellipsis" $ do
        -- pattern (hansi ((name ...) (value ...)) ...) / application (hansi ((1 2 3) (4 5 6)) ((3 2 1) (6 5 4)))
        let bindingTree = [SubPatternEllipsis (ApplicationNode (ApplicationNode (IdentifierAtom "name") [IdentifierAtom "..."]) [ApplicationNode (IdentifierAtom "value") [IdentifierAtom "..."]]) [[IdentifierEllipsis (IdentifierAtom "name") [NumberAtom (Exact (Integer 1)),NumberAtom (Exact (Integer 2)),NumberAtom (Exact (Integer 3))],IdentifierEllipsis (IdentifierAtom "value") [NumberAtom (Exact (Integer 4)),NumberAtom (Exact (Integer 5)),NumberAtom (Exact (Integer 6))]],[IdentifierEllipsis (IdentifierAtom "name") [NumberAtom (Exact (Integer 3)),NumberAtom (Exact (Integer 2)),NumberAtom (Exact (Integer 1))],IdentifierEllipsis (IdentifierAtom "value") [NumberAtom (Exact (Integer 6)),NumberAtom (Exact (Integer 5)),NumberAtom (Exact (Integer 4))]]]]
        -- template (list (list name ...) ...)
        let template = TemplateListNode [TemplateIdentifierAtom [1] "list",TemplateEllipsisNode 1 [0] (TemplateListNode [TemplateIdentifierAtom [0,1] "list",TemplateEllipsisNode 1 [0,0] (TemplateIdentifierAtom [0,0] "name")])]
        -- result (list (b a b matcha) (d c b matcha))
        transform bindingTree template `shouldBe` 
          TemplateListNode [
            TemplateIdentifierAtom [1] "list",
            TemplateListNode [
              TemplateIdentifierAtom [0,1] "list",
              TemplateAtom (NumberAtom (Exact (Integer 1)))
              TemplateAtom (NumberAtom (Exact (Integer 2)))
              TemplateAtom (NumberAtom (Exact (Integer 3)))
            ],
            TemplateListNode [
              TemplateIdentifierAtom [0,1] "list",
              TemplateAtom (NumberAtom (Exact (Integer 3)))
              TemplateAtom (NumberAtom (Exact (Integer 2)))
              TemplateAtom (NumberAtom (Exact (Integer 1)))
            ]
          ]


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

    describe "MacroEngine.analyseTemplateList" $ do
      it "resolves single ellipsis" $ do
        analyseTemplateList ellipsis [] [IdentifierAtom "a", IdentifierAtom "...", IdentifierAtom "b"] `shouldBe` [TemplateEllipsisNode 1 [1] (TemplateIdentifierAtom [1] "a"), TemplateIdentifierAtom [0] "b"]

      it "resolves double ellipsis" $ do
        analyseTemplateList ellipsis [] [IdentifierAtom "a", IdentifierAtom "...", IdentifierAtom "...", IdentifierAtom "b"] `shouldBe` [TemplateEllipsisNode 2 [1] (TemplateIdentifierAtom [1] "a"), TemplateIdentifierAtom [0] "b"]

    describe "MacroEngine.lookup" $ do
      it "looks up simple ellipse correctly" $ do
        bindingLookup 1 "name" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "name") [IdentifierAtom "value"])
          [
            [Value (IdentifierAtom "name") (StringAtom "rap"), Value (IdentifierAtom "value") (StringAtom "gru")],
            [Value (IdentifierAtom "name") (StringAtom "gru"), Value (IdentifierAtom "value") (StringAtom "rap")]
          ]]
        `shouldBe`
        Just [StringAtom "rap", StringAtom "gru"]
      it "looks up double ellipse correctly" $ do
        bindingLookup 2 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e") [IdentifierAtom "..."])
          [
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "c", StringAtom "d"]]
          ]]
        `shouldBe`
        Just [StringAtom "a", StringAtom "b", StringAtom "c", StringAtom "d"]
      it "throws error when template has not enough ellipses" $ do
        evaluate (bindingLookup 1 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e") [IdentifierAtom "..."])
          [
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "c", StringAtom "d"]]
          ]])
        `shouldThrow`
        anyErrorCall
      it "throws error when template has too many ellipses" $ do
        evaluate (bindingLookup 3 "e" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e") [IdentifierAtom "..."])
          [
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "c", StringAtom "d"]]
          ]])
        `shouldThrow`
        anyErrorCall
      it "returns Nothing if identifier is a free reference" $ do
        bindingLookup 2 "free" [SubPatternEllipsis 
          (ApplicationNode (IdentifierAtom "e") [IdentifierAtom "..."])
          [
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "a", StringAtom "b"]],
            [IdentifierEllipsis (IdentifierAtom "e") [StringAtom "c", StringAtom "d"]]
          ]]
        `shouldBe`
        Nothing