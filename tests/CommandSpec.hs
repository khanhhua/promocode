{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Command ( conditionalPromotion )
import Data.Transaction ( Transaction(Transaction), Item(Item) )
import Lang
    ( Condition(Product, Sum), Promotion(..), Action (TransactionDiscount) )
import Data.Offer (Offer(..))

transaction :: Transaction
transaction = Transaction [ Item "P901" 4 499
                          ]

spec :: Spec
spec = do
  describe "process Transaction Item Product" $ do
    it "should execute action when condition is met" $ do
      let condition = Product "P901" 4
          action = TransactionDiscount 0.01
          promotion = conditionalPromotion condition action
          actual = runPromotion promotion transaction
      actual `shouldBe` Just (Discount 4.99)

    it "should not execute action due to unmet condition" $ do
      let condition = Product "P901" 5
          action = TransactionDiscount 0.01
          promotion = conditionalPromotion condition action
          actual = runPromotion promotion transaction
      actual `shouldBe` Nothing
  describe "process Transaction Total" $ do
    it "should execute action when condition is met" $ do
      let condition = Sum 498
          action = TransactionDiscount 0.01
          promotion = conditionalPromotion condition action
          actual = runPromotion promotion transaction
      actual `shouldBe` Just (Discount 4.99)

    it "should not execute action due to unmet condition" $ do
      let condition = Sum 500
          action = TransactionDiscount 0.01
          promotion = conditionalPromotion condition action
          actual = runPromotion promotion transaction
      actual `shouldBe` Nothing
