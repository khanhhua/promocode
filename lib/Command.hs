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
    Condition (Product),
    Promotion (..),
  )

type Logs = [String]

type Conditional = Reader Transaction Bool

type Command = Reader Transaction Float

doTxCondition :: Condition -> Conditional
doTxCondition condition = do
  (Transaction items) <- ask
  return $ any (matchProductItem condition) items

doTxAction :: Action -> Command
doTxAction (TransactionDiscount rate) = do
  tx <- ask
  return $ T.total tx * rate
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  return $ max (T.total tx) amount

process :: Promotion -> Reader Transaction (Maybe Float)
process (Promotion condition action) = do
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