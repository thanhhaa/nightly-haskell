module Lecture7.ModelingBoxes where

import Text.Printf (printf)

newtype Money = Money Int
  deriving (Show)

-- Problem: Type ambiguity in division
--
-- renderMoney :: Money -> String
-- renderMoney (Money cents) = show (fromIntegral cents / 100)
--
-- • Defaulting the type variable ‘a0’ to type ‘Double’ in the following constraints
--    (Show a0)
--      arising from a use of ‘show’
--      at src/Lecture7/ModelingBoxesNCases.hs:7:29-32
--    (Fractional a0)
--      arising from a use of ‘/’
--      at src/Lecture7/ModelingBoxesNCases.hs:7:54
--    (Num a0)
--      arising from a use of ‘fromIntegral’
--      at src/Lecture7/ModelingBoxesNCases.hs:7:35-46
-- • In the expression: show (fromIntegral cents / 100)
--   In an equation for ‘renderMoney’:
--      renderMoney (Money cents) = show (fromIntegral cents / 100)
--
-- Explanation error:
--   Haskell's type system is very strict about numeric types.
--   When you write fromIntegral cents / 100, Haskell sees multiple possible types
--   that could work (Float, Double, Rational, etc.) and can't decide which one you want.
--
-- SOLUTION 1: Explicit type annotation
-- renderMoney1 :: Money -> String
-- renderMoney1 (Money cents) = show (fromIntegral cents / 100 :: Double)
--
-- SOLUTION 2: Use specific conversion function
-- renderMoney2 :: Money -> String
-- renderMoney2 (Money cents) = show (fromIntegral cents / 100.0)
--
-- SOLUTION 3: More precise formatting with printf
-- import Text.Printf
--
-- renderMoney3 :: Money -> String
-- renderMoney3 (Money cents) = printf "%.2f" (fromIntegral cents / 100 :: Double)
--
-- SOLUTION 4: Custom formatting without division
-- renderMoney4 :: Money -> String
-- renderMoney4 (Money cents) =
--   let dollars = cents `div` 100
--       remainingCents = cents `mod` 100
--   in show dollars ++ "." ++ printf "%02d" remainingCents
--
-- SOLUTION 5: Using rational numbers for exact precision
-- import Data.Ratio
--
-- renderMoney5 :: Money -> String
-- renderMoney5 (Money cents) =
--   let rational = fromIntegral cents % 100
--       dollars = numerator rational `div` denominator rational
--       remainingCents = (numerator rational `mod` denominator rational) * 100 `div` denominator rational
--   in show dollars ++ "." ++ printf "%02d" remainingCents
--
-- SOLUTION 6: Enhanced Money type with better operations
-- newtype Money = Money Int
--   deriving (Show, Eq, Ord)
--
renderMoney :: Money -> String
renderMoney (Money cents) = show (fromIntegral cents / 100 :: Double)

renderMoney4 :: Money -> String
renderMoney4 (Money cents) =
  let dollars = cents `div` 100
      remainingCents = cents `mod` 100
   in show dollars ++ "." ++ printf "%02d" remainingCents

(+!) :: Money -> Money -> Money
(Money a) +! (Money b) = Money (a + b)

scale :: Money -> Double -> Money
scale (Money a) x = Money (round (fromIntegral a * x))

addVat :: Money -> Money
addVat m = m +! scale m 0.24

-- Example usage
main :: IO ()
main = do
  let money1 = Money 1250

  putStrLn $ "Money 1: " ++ renderMoney money1
  putStrLn $ "Money 4: " ++ renderMoney4 money1

  -- Different rendering approaches
  putStrLn "\nDifferent rendering methods:"
  putStrLn $ "Method 1: " ++ renderMoney money1
  putStrLn $ "Method 4: " ++ renderMoney4 money1