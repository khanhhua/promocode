module Command
  ( doTxCondition,
    doTxAction,
    process,
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
  )

type Logs = [String]

type Conditional = Reader Transaction Bool

type Command = Reader Transaction Float

doTxCondition :: Condition -> Conditional
doTxCondition condition@(Product _ _) = do
  (Transaction items) <- ask
  return $ any (matchProductItem condition) items

doTxCondition condition@(Sum minTotal) = do
  tx <- ask
  return $ total tx >= minTotal

doTxAction :: Action -> Command
doTxAction (TransactionDiscount rate) = do
  tx <- ask
  return $ T.total tx * rate
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  return $ max (T.total tx) amount

process :: Promotion -> Transaction -> Maybe Float
process (Promotion condition action) = runReader reader
  where
    reader :: Reader Transaction (Maybe Float)
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