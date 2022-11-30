module Command where

import Data.Transaction
import Lang

type Logs = [String]


process :: Promotion -> Transaction -> Maybe Float
process (Promotion condition action) t@(Transaction items) =
  if any (matchProductItem condition) items then
    let fn = getTransactionDiscount action
    in Just $ fn t
  else Nothing


matchProductItem :: Condition -> Item -> Bool
matchProductItem (Product conditionProductId conditionMinQty) item =
  conditionProductId == itemProductId item && itemQty item >= conditionMinQty

matchProductItem _ _ = False