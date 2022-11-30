module Data.Transaction where

data Item = Item
  { itemProductId :: Int
  , itemQty :: Int
  , itemSubtotal :: Float
  }

newtype Transaction = Transaction [Item]