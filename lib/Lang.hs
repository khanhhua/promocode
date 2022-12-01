module Lang where

import Data.Transaction

data Condition
  = Product {productId :: Int, minQty :: Int}
  | Sum {minTotal :: Float}

data Action
  = TransactionDiscount Float
  | ConcreteDiscount Float

-- | ItemDiscount { runItemDiscount :: Item -> Float }
data Promotion = Promotion !Condition !Action
