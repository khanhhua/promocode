module CommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Command ( process )
import Data.Transaction ( Transaction(Transaction), Item(Item) )
import Lang
    ( Condition(Product, Sum), Promotion(Promotion), Action (TransactionDiscount) )
import Control.Monad.Reader (runReader)

transaction :: Transaction
transaction = Transaction [ Item 901 4 499
                          ]

spec :: Spec
spec = do
  describe "process Transaction Item Product" $ do
    it "should execute action when condition is met" $ do
      let condition = Product 901 4
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runReader (process promotion) transaction
      actual `shouldBe` Just 4.99

    it "should not execute action due to unmet condition" $ do
      let condition = Product 901 5
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runReader (process promotion) transaction
      actual `shouldBe` Nothing
  describe "process Transaction Total" $ do
    it "should execute action when condition is met" $ do
      let condition = Sum 498
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runReader (process promotion) transaction
      actual `shouldBe` Just 4.99

    it "should not execute action due to unmet condition" $ do
      let condition = Sum 500
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runReader (process promotion) transaction
      actual `shouldBe` Nothing
