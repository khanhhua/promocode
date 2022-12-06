module Data.Offer where

data Offer
  = Present String
  | Discount Float
  deriving (Show, Eq)

instance Ord Offer where
  Present a <= Present b = a <= b
  Present a <= _ = False
  Discount a <= Discount b = a <= b
  Discount a <= _ = True