module Data.StackSpec where

import qualified Data.Stack as Stack

import Test.Hspec

spec :: Spec
spec = describe "Stack" $ do
  describe "Stack.empty" $ do
    it "can create empty stack" $ do
      (Stack.empty :: Stack.Stack ()) `shouldBe` (Stack.empty :: Stack.Stack ())

  describe "Stack.fromList" $ do
    it "can create stack from list" $ do
      Stack.fromList [3,5] `shouldBe` Stack.fromList [3,5]

    it "can create empty stack from list" $ do
      (Stack.fromList [] :: Stack.Stack ()) `shouldBe` (Stack.empty :: Stack.Stack ())

    it "can create singleton stack from list" $ do
      Stack.fromList [1] `shouldBe` Stack.singleton 1

  describe "Stack.singleton" $ do
    it "can create singleton stack" $ do
      Stack.singleton 3 `shouldBe` Stack.singleton 3
  
  describe "Stack.push" $ do
    it "can push to empty stack" $ do
      Stack.push (Stack.empty) 3 `shouldBe` Stack.singleton 3

    it "can push to populated stack" $ do
      Stack.push (Stack.fromList [3,2,1]) 4 `shouldBe` Stack.fromList [4,3,2,1]

  describe "Stack.pop" $ do
    it "can pop empty from empty stack" $ do
      Stack.pop (Stack.empty :: Stack.Stack ()) `shouldBe` (Nothing, Stack.empty :: Stack.Stack ())

    it "can pop from singleton stack" $ do
      Stack.pop (Stack.singleton 1) `shouldBe` (Just 1, Stack.empty :: Stack.Stack Int)

    it "can pop from populated stack" $ do
      Stack.pop (Stack.fromList [3,2,1]) `shouldBe` (Just 3, Stack.fromList [2,1])

    it "push / pop idempotency" $ do
      (Stack.pop (Stack.push (Stack.fromList [3,2,1]) 4)) `shouldBe` (Just 4, Stack.fromList [3,2,1])
