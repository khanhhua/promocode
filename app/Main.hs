{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Lang (Promotion(Promotion, runPromotion), Action (TransactionDiscount), Condition (Sum, productId))
import Control.Monad.Reader (runReader)
import Data.Transaction (Transaction(..), Item (Item))
import Command (conditionalPromotion)
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import System.IO (stdin, stdout)
import Data.Offer (Offer)


type Logs = [String]

productCatalog :: [([Char], Float)]
productCatalog =
    [ ("RADIO", 29.0)
    , ("TV", 459.0)
    , ("SOFA", 349.0)
    ]

condition :: Condition
condition = Sum 498

action :: Action Offer
action = TransactionDiscount 0.01

promotion :: Promotion Offer
promotion = conditionalPromotion condition action

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  productId <- putStr "Product: " >> getLine
  qty :: Integer <- read <$> (putStr "Qty: " >> getLine)
  let maybeSubtotal = (* fromInteger qty) <$> lookup productId productCatalog
      maybeTx = (\subtotal -> Transaction [Item productId qty subtotal]) <$> maybeSubtotal

      maybeDiscount = maybeTx >>= runPromotion promotion

  case maybeDiscount of
    Nothing -> putStrLn "You have no promotion!"
    Just discount -> putStrLn ("Promotion: " <> show discount)
