module Command
  ( conditionalPromotion,
    betterPromotion,
    bestPromotion
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
    ConditionalPromotion,
    PromotionT,
    runPromotion
  )
import Data.Offer (Offer (Discount))
import qualified Data.Functor
import Data.Maybe (isNothing)

type Logs = [String]

type TxAction a = Action a -> PromotionT a

doTxCondition :: Condition -> PromotionT Bool
doTxCondition condition@(Product _ _) = do
  (Transaction items) <- ask
  pure . Just $ any (matchProductItem condition) items

doTxCondition condition@(Sum minTotal) = do
  tx <- ask
  pure . Just $ total tx >= minTotal

doTxAction :: TxAction Offer
doTxAction (GiveOffer offer) = pure . Just $ offer

doTxAction (TransactionDiscount rate) = do
  tx <- ask
  pure . Just $ Discount (T.total tx * rate)
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  pure . Just $ Discount (max (T.total tx) amount)


conditionalPromotion :: Condition -> Action Offer -> PromotionT Offer
conditionalPromotion condition action = do
  maybeBool <- doTxCondition condition
  case maybeBool of
    Just True -> doTxAction action
    _ -> pure Nothing


betterPromotion :: PromotionT Offer -> PromotionT Offer -> PromotionT Offer
betterPromotion promotionA promotionB = do
  bestPromotion [promotionA, promotionB]

bestPromotion :: [PromotionT Offer] -> PromotionT Offer
bestPromotion = foldM f Nothing
  where
    f acc m = do
      offer <- m
      if isNothing offer then
        pure acc
      else
        pure $ max acc offer


matchProductItem :: Condition -> Item -> Bool
matchProductItem (Product productId minQty) item =
  productId == T.productId item && T.qty item >= minQty
matchProductItem _ _ = False