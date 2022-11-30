{-# LANGUAGE StrictData #-}
module Lang where

import Data.Transaction

data Condition
  = Product { productId :: Int, minQty :: Int }
  | Sum { minTotal :: Float }

newtype Action = TransactionDiscount { getTransactionDiscount :: Transaction -> Float }
-- | ItemDiscount { runItemDiscount :: Item -> Float }

data Promotion = Promotion !Condition !Action


transactionDiscount :: Action
transactionDiscount = TransactionDiscount (const 1)

-- itemDiscount :: Action
-- itemDiscount = ItemDiscount (const 2)