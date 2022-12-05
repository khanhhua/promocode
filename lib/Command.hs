module Command
  ( doTxCondition,
    doTxAction,
    conditionalPromotion,
    Conditional,
    Command,
  )
where

import Control.Monad.Reader
import Data.Transaction as T
  ( Item (productId, qty),
    Transaction (..),
    total,
  )
import Lang
  ( Action (..),
    Condition (Product, Sum),
    Promotion (..),
    ConditionalPromotion,
  )
import Data.Offer (Offer (Discount))

type Logs = [String]

type Conditional = Reader Transaction Bool

type Command a = Reader Transaction a

type TxAction a = Action a -> Command a

doTxCondition :: Condition -> Conditional
doTxCondition condition@(Product _ _) = do
  (Transaction items) <- ask
  return $ any (matchProductItem condition) items

doTxCondition condition@(Sum minTotal) = do
  tx <- ask
  return $ total tx >= minTotal

doTxAction :: TxAction Offer
doTxAction (GiveOffer offer) = pure offer

doTxAction (TransactionDiscount rate) = do
  tx <- ask
  return $ Discount (T.total tx * rate)
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  return $ Discount (max (T.total tx) amount)


conditionalPromotion :: ConditionalPromotion Offer
conditionalPromotion condition action = Promotion f
  where
    f :: Transaction -> Maybe Offer
    f = runReader reader
    reader = do
      bool <- doTxCondition condition
      if bool
        then do
          Just <$> doTxAction action
        else do
          return Nothing


matchProductItem :: Condition -> Item -> Bool
matchProductItem (Product productId minQty) item =
  productId == T.productId item && T.qty item >= minQty
matchProductItem _ _ = False