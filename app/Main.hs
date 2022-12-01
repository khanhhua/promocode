{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Lang (Promotion(Promotion), Action (TransactionDiscount), Condition (Sum, productId))
import Control.Monad.Reader (runReader)
import Data.Transaction (Transaction(..), Item (Item))
import Command (process)
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import System.IO (stdin, stdout)


type Logs = [String]

productCatalog :: [([Char], Float)]
productCatalog =
    [ ("RADIO", 29.0)
    , ("TV", 459.0)
    , ("SOFA", 349.0)
    ]

condition :: Condition
condition = Sum 498

action :: Action
action = TransactionDiscount 0.01

promotion :: Promotion
promotion = Promotion condition action

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  productId <- putStr "Product: " >> getLine
  qty :: Integer <- read <$> (putStr "Qty: " >> getLine)
  let maybeSubtotal = (* fromInteger qty) <$> lookup productId productCatalog
      maybeTx = (\subtotal -> Transaction [Item productId qty subtotal]) <$> maybeSubtotal

      maybeDiscount = maybeTx >>= process promotion

  case maybeDiscount of
    Nothing -> putStrLn "You have no promotion!"
    Just discount -> putStrLn ("Promotion: " <> show discount)
