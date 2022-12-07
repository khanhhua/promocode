module Command
  ( App(..)
  , runApp
  , TxAction
  , TxCondition
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
    PromotionT,
    runPromotion, Promotion(..), ConditionalPromotion
  )
import qualified Data.Functor
import Data.Maybe (isNothing)

type TxAction a = Action a -> PromotionT a
type TxCondition = Condition -> PromotionT Bool

data (Ord a) => App a = App
  { handleAction :: TxAction a
  , promotions :: [Promotion a]
  }


runApp :: (Ord a) => App a -> Transaction -> Maybe a
runApp (App doTxAction promotions) =
  let conditionalPromotions = map (makeConditionalPromotion doTxAction) promotions
  in runPromotion (bestPromotion conditionalPromotions)


makeConditionalPromotion :: TxAction a -> Promotion a -> PromotionT a
makeConditionalPromotion doTxAction (Promotion condition action) = do
  maybeBool <- doTxCondition condition
  case maybeBool of
    Just True -> doTxAction action
    _ -> pure Nothing


bestPromotion :: (Ord a) => [PromotionT a] -> PromotionT a
bestPromotion = foldM f Nothing
  where
    f acc m = do
      offer <- m
      if isNothing offer then
        pure acc
      else
        pure $ max acc offer


doTxCondition :: TxCondition
doTxCondition condition@(Product _ _) = do
  (Transaction items) <- ask
  pure . Just $ any (matchProductItem condition) items

doTxCondition condition@(Sum minTotal) = do
  tx <- ask
  pure . Just $ total tx >= minTotal


matchProductItem :: Condition -> Item -> Bool
matchProductItem (Product productId minQty) item =
  productId == T.productId item && T.qty item >= minQty
matchProductItem _ _ = False
