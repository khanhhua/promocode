module Lang where

import Data.Transaction

data Condition
  = Product {productId :: String, minQty :: Integer}
  | Sum {minTotal :: Float}

data Action
  = TransactionDiscount Float
  | ConcreteDiscount Float

-- | ItemDiscount { runItemDiscount :: Item -> Float }
data Promotion = Promotion !Condition !Action
