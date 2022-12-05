{-# LANGUAGE StrictData #-}
module Lang where

import Data.Transaction

data Condition
  = Product {productId :: String, minQty :: Integer}
  | Sum {minTotal :: Float}

data Action a
  = GiveOffer a
  | TransactionDiscount Float
  | ConcreteDiscount Float

{-| Promotion of "a" is a transformation from transaction to some offer "a"
-}
newtype Promotion a = Promotion 
  { runPromotion :: Transaction -> Maybe a
  }

type ConditionalPromotion a = Condition -> Action a -> Promotion a