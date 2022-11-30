module CommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Command ( process )
import Data.Transaction ( Transaction(Transaction), Item(Item) )
import Lang
    ( Condition(Product), transactionDiscount, Promotion(Promotion) )

transaction :: Transaction
transaction = Transaction [ Item 901 4 499
                          ]

spec :: Spec
spec = do
  describe "process" $ do
    it "should execute action when condition is met" $ do
      let condition = Product 901 4
          action = transactionDiscount
          promotion = Promotion condition action
          actual = process promotion transaction
      actual `shouldBe` Just 1

    it "should not execute action due to unmet condition" $ do
      let condition = Product 901 5
          action = transactionDiscount
          promotion = Promotion condition action
          actual = process promotion transaction
      actual `shouldBe` Nothing

