{-# LANGUAGE ScopedTypeVariables #-}

module CommandSpec where

import Command (App (..), TxAction, runApp)
import Control.Monad.Reader (MonadReader (ask))
import Data.Transaction (Item (Item), Transaction (Transaction))
import qualified Data.Transaction as T
import Lang
  ( Action (ConcreteDiscount, GiveOffer, TransactionDiscount),
    Condition (Product, Sum),
    Promotion (Promotion),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

data MockOffer
  = Present String
  | Discount Float
  deriving (Show, Eq)

instance Ord MockOffer where
  Present a <= Present b = a <= b
  Present _ <= _ = False
  Discount a <= Discount b = a <= b
  Discount _ <= _ = True

transaction :: Transaction
transaction =
  Transaction
    [ Item "P901" 4 499
    ]

doTxAction :: TxAction MockOffer
doTxAction (GiveOffer offer) = pure . Just $ offer
doTxAction (TransactionDiscount rate) = do
  tx <- ask
  pure . Just $ Discount (T.total tx * rate)
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  pure . Just $ Discount (max (T.total tx) amount)

spec :: Spec
spec = do
  describe "process Transaction Item Product" $ do
    it "should execute action when condition is met" $ do
      let condition = Product "P901" 4
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runApp (App doTxAction [promotion]) transaction
      actual `shouldBe` Just (Discount 4.99)

    it "should not execute action due to unmet condition" $ do
      let condition = Product "P901" 5
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runApp (App doTxAction [promotion]) transaction
      actual `shouldBe` Nothing
  describe "process Transaction Total" $ do
    it "should execute action when condition is met" $ do
      let condition = Sum 498
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runApp (App doTxAction [promotion]) transaction
      actual `shouldBe` Just (Discount 4.99)

    it "should not execute action due to unmet condition" $ do
      let condition = Sum 500
          action = TransactionDiscount 0.01
          promotion = Promotion condition action
          actual = runApp (App doTxAction [promotion]) transaction
      actual `shouldBe` Nothing

  describe "choose better of two promotion" $ do
    it "should choose higher discount" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = Promotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = TransactionDiscount 0.02
          promotionB = Promotion conditionB actionB

          actual =
            runApp
              ( App
                  doTxAction
                  [ promotionA,
                    promotionB
                  ]
              )
              transaction
      actual `shouldBe` Just (Discount $ 499 * 0.02)

    it "should choose the present (cuz it's a cat)" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = Promotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = GiveOffer (Present "Cat")
          promotionB = Promotion conditionB actionB

          actual =
            runApp
              ( App
                  doTxAction
                  [ promotionA,
                    promotionB
                  ]
              )
              transaction
      actual `shouldBe` Just (Present "Cat")

  describe "choose best of three promotions" $ do
    it "should choose higher discount" $ do
      let conditionA = Sum 498
          actionA = TransactionDiscount 0.01
          promotionA = Promotion conditionA actionA

          conditionB = Product "P901" 4
          actionB = TransactionDiscount 0.02
          promotionB = Promotion conditionB actionB

          conditionC = Product "P901" 4
          actionC = GiveOffer (Present "Cat")
          promotionC = Promotion conditionC actionC

          actual =
            runApp
              ( App
                  doTxAction
                  [ promotionA,
                    promotionB,
                    promotionC
                  ]
              )
              transaction
      actual `shouldBe` Just (Present "Cat")