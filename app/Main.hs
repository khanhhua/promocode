{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Lang (PromotionT, Action (..), Condition (Sum, Product, productId), runPromotion)
import Control.Monad.Reader (runReader)
import Data.Transaction (Transaction(..), Item (Item))
import Command (conditionalPromotion, bestPromotion)
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import System.IO (stdin, stdout)

import Data.Offer (Offer(..))


type Logs = [String]

productCatalog :: [([Char], Float)]
productCatalog =
    [ ("RADIO", 29.0)
    , ("TV", 459.0)
    , ("SOFA", 349.0)
    ]

promotionA, promotionB, promotionC :: PromotionT Offer
promotionA = conditionalPromotion (Sum 498) (TransactionDiscount 0.01)
promotionB = conditionalPromotion (Product "TV" 1) (GiveOffer $ Present "Cleaner")
promotionC = conditionalPromotion (Product "RADIO" 30) (TransactionDiscount 0.05)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  productId <- putStr "Product: " >> getLine
  qty :: Integer <- read <$> (putStr "Qty: " >> getLine)
  let maybeSubtotal = (* fromInteger qty) <$> lookup productId productCatalog
      maybeTx = (\subtotal -> Transaction [Item productId qty subtotal]) <$> maybeSubtotal
      promotion = bestPromotion [ promotionA
                                , promotionB
                                , promotionC
                                ]
      maybeDiscount = runPromotion promotion <$> maybeTx

  case maybeDiscount of
    Nothing -> putStrLn "You have no promotion!"
    Just discount -> putStrLn ("Promotion: " <> show discount)
