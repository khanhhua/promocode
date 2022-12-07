{-# LANGUAGE StrictData #-}
module Lang where

import Data.Transaction
import Control.Monad.RWS (MonadReader)
import Control.Monad.Reader (ReaderT (runReaderT), Reader, runReader)
import qualified Control.Monad.Identity as Data.Functor.Identity
import Control.Monad.Identity (Identity)

data Condition
  = Product {productId :: String, minQty :: Integer}
  | Sum {minTotal :: Float}

data Action a
  = GiveOffer a
  | TransactionDiscount Float
  | ConcreteDiscount Float

data Promotion a = Promotion Condition (Action a)

{-|
Promotion of "a" is a transformation from transaction to some offer "a"
-}
type PromotionT a = Reader Transaction (Maybe a)

type ConditionalPromotion a = Action a -> PromotionT a

runPromotion :: PromotionT a -> Transaction -> Maybe a
runPromotion = runReader
