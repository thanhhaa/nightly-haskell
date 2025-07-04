module Main where

import Data.Text (Text)
import GHC.Generics (Generic)

data Discount
  = DiscountPercent Int -- A percentage discount
  | DiscountConstant Int -- A constant discount
  | MinimumPrice Int -- Set a minimum price
  | ForCustomer String Discount -- Discounts can be conditional
  | Many [Discount] -- Apply a number of discounts in row

applyDiscount :: String -> Int -> Discount -> Int
applyDiscount _ price (DiscountPercent percent) = price - (price * percent) `div` 100
applyDiscount _ price (DiscountConstant constant) = price - constant
applyDiscount _ price (MinimumPrice minPrice) = max price minPrice
applyDiscount customer price (ForCustomer target discount)
  | customer == target = applyDiscount customer price discount
  | otherwise = price
applyDiscount customer price (Many discounts) = go price discounts
 where
  go p [] = p
  go p (d : ds) = go (applyDiscount customer p d) ds

data CreateTodo = CreateTodo
  { createTodoTitle :: Text
  , createTodoDescription :: Maybe Text
  }
  deriving (Show, Generic)

main :: IO ()
main = do
  putStrLn ("applyDiscount \"Bod\" 120 (DiscountPercent 50) = " ++ show (applyDiscount "Bod" 120 (DiscountPercent 50)))
  putStrLn ("applyDiscount \"Bod\" 60 (DiscountConstant 30) = " ++ show (applyDiscount "Bod" 60 (DiscountConstant 30)))
  putStrLn ("applyDiscount \"Bod\" 30 (MinimumPrice 35) = " ++ show (applyDiscount "Bod" 30 (MinimumPrice 35)))
  putStrLn ("applyDiscount \"Yvonne\" 100 (Many [ForCustomer \"Yvonne\" (DiscountConstant 10), ForCustomer \"Ssarah\" (DiscountConstant 20)]) = " ++ show (applyDiscount "Yvonne" 100 (Many [ForCustomer "Yvonne" (DiscountConstant 10), ForCustomer "Ssarah" (DiscountConstant 20)])))
  putStrLn ("applyDiscount \"Ssarah\" 100 (Many [ForCustomer \"Yvonne\" (DiscountConstant 10), ForCustomer \"Ssarah\" (DiscountConstant 20)]) = " ++ show (applyDiscount "Ssarah" 100 (Many [ForCustomer "Yvonne" (DiscountConstant 10), ForCustomer "Ssarah" (DiscountConstant 20)])))