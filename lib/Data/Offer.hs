module Data.Offer where

data Offer
  = Present String
  | Discount Float
  deriving (Show, Eq)