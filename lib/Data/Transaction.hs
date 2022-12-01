module Data.Transaction where

data Item = Item
  { productId :: String
  , qty :: Integer
  , subtotal :: Float
  }

newtype Transaction = Transaction [Item]

total :: Transaction -> Float
total (Transaction items) =
  foldl (\acc item -> subtotal item + acc) 0 items