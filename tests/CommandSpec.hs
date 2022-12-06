{-# LANGUAGE ScopedTypeVariables #-}
module CommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Command (conditionalPromotion, betterPromotion, bestPromotion)
import Data.Transaction ( Transaction(Transaction), Item(Item) )
import Lang
    ( Condition(Product, Sum), Action (TransactionDiscount, GiveOffer), runPromotion )
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

  describe "choose better of two promotion" $ do
    it "should choose higher discount" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = conditionalPromotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = TransactionDiscount 0.02
          promotionB = conditionalPromotion conditionB actionB

          actual = runPromotion (betterPromotion promotionA promotionB) transaction
      actual `shouldBe` Just (Discount $ 499 * 0.02)

    it "should choose the present (cuz it's a cat)" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = conditionalPromotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = GiveOffer (Present "Cat")
          promotionB = conditionalPromotion conditionB actionB

          actual = runPromotion (betterPromotion promotionA promotionB) transaction
      actual `shouldBe` Just (Present "Cat")

  describe "choose best of three promotions" $ do
    it "should choose higher discount" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = conditionalPromotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = TransactionDiscount 0.02
          promotionB = conditionalPromotion conditionB actionB

          conditionC = Product "P901" 4
          actionC = GiveOffer (Present "Cat")
          promotionC = conditionalPromotion conditionC actionC

          actual = runPromotion (bestPromotion [promotionA, promotionB, promotionC]) transaction
      actual `shouldBe` Just (Present "Cat")