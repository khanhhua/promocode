{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import Data.Maybe

import Lang (PromotionT, Action (..), Condition (Sum, Product, productId), runPromotion, Promotion (Promotion))
import Data.Transaction as T
    ( Item(Item, productId, qty), Transaction(Transaction), total )
import Command (App(..), runApp, TxAction, TxCondition)
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import System.IO (stdin, stdout)

import Data.Offer (Offer(..))


productCatalog :: [([Char], Float)]
productCatalog =
    [ ("RADIO", 29.0)
    , ("TV", 459.0)
    , ("SOFA", 349.0)
    ]

app = App
  { handleAction = doTxAction
  , promotions = [ Promotion (Sum 498) (TransactionDiscount 0.01)
                 , Promotion (Product "TV" 1) (GiveOffer $ Present "Cleaner")
                 , Promotion (Product "RADIO" 30) (TransactionDiscount 0.05)
                 ]
  }


doTxAction :: TxAction Offer
doTxAction (GiveOffer offer) = pure . Just $ offer
doTxAction (TransactionDiscount rate) = do
  tx <- ask
  pure . Just $ Discount (T.total tx * rate)
doTxAction (ConcreteDiscount amount) = do
  tx <- ask
  pure . Just $ Discount (max (T.total tx) amount)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  productId <- putStr "Product: " >> getLine
  qty :: Integer <- read <$> (putStr "Qty: " >> getLine)
  let maybeSubtotal = (* fromInteger qty) <$> lookup productId productCatalog
      maybeTx = (\subtotal -> Transaction [Item productId qty subtotal]) <$> maybeSubtotal

      maybeDiscount = maybeTx >>= runApp app

  case maybeDiscount of
    Nothing -> putStrLn "You have no promotion!"
    Just discount -> putStrLn ("Promotion: " <> show discount)