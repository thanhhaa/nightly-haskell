module Lecture7.Semigroups where

{-

------------Semigroups------------------
Mathematically speaking, an associative function (or operator) forms a semigroup.
Haskell has a type class Semigroup (defined in the module Data.Semigroup)
that can be used when a type has one clear associative operation.

-- What is the associative/əˈsəʊ.ʃi.ə.tɪv/?
Associative means that when you apply an operation multiple times, the grouping
doesn't matter:
          (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
  - The parentheses can be moved around without changing the result

-- What is a Semigroup?
A semigroup is a mathenmatical structure consisting of:
  1. A set of elements
  2. An associative binary operation(Một phép toán nhị phân có tính kết hợp)

Examples of associative
- Associative operations(Semigroup)
  - Addition:             (1 + 2) + 3 = 1 + (2 + 3)
  - Multiplication:       (2 * 3) * 4 = 2 * (3 * 4)
  - String concatenation: ("Hello" + " ") + "World" = "Hello" + (" " + "World")
  - List concatenation:   ([1, 2] ++ [3]) ++ [4] = [1, 2] ++ ([3] ++ [4])
- Non-associative operations(Not Semigroup)
  - Subtraction:    (10 - 5) - 2 != 10 - (5 - 2)
  - Division:       (24 / 4) / 2 != 24 / (4 / 2)
  - Exponent: (2³)² != 2^(3²)

--
-- Type Class Semigroup(from the module Data.Semigroup) in Haskell

    class Semigroup a where
      (<>) :: a -> a -> a  -- associative operation

  Semigroup provides a foundation for many functional programming patterns.
  It allows us to abstract over any "combining" operation,
  making code more reusable and composable.
--

-}

import Data.Semigroup (Sum (..))

data ShoppingCart = ShoppingCart
  { items :: [String]
  , totalPrice :: Sum Double
  , itemCount :: Sum Int
  }
  deriving (Show)

instance Semigroup ShoppingCart where
  cart1 <> cart2 =
    ShoppingCart
      { items = items cart1 <> items cart2
      , totalPrice = totalPrice cart1 <> totalPrice cart2
      , itemCount = itemCount cart1 <> itemCount cart2
      }

mainL7M :: IO ()
mainL7M = do
  putStrLn "---Shopping Cart Semigroup---"
  let cart1 = ShoppingCart ["Apple", "Banana"] (Sum 5.0) (Sum 2)
  let cart2 = ShoppingCart ["Orange"] (Sum 3.0) (Sum 1)
  let cart3 = ShoppingCart ["Milk", "Bread"] (Sum 8.0) (Sum 2)
  let combinedCart = cart1 <> cart2 <> cart3
  print combinedCart

  {-
  warning: [GHC-18042] [-Wtype-defaults]
      • Defaulting the type variable ‘a0’ to type ‘Integer’ in the following constraint
          Num a0 arising from the literal ‘3’
      • In the first argument of ‘Sum’, namely ‘3’
        In the expression: Sum 3
        In an equation for ‘c3’: c3 = Sum 3
     |
  83 |   let c3 = Sum 3

  Warning này xuất hiện vì literal 3 có kiểu đa hình (polymorphic) - nó có thể là Int, Integer, Double, etc.
  GHC không biết chính xác kiểu nào bạn muốn nên mặc định chọn Integer.
  -}

  putStrLn "\n---Testing Associative---"
  let a1 = Sum (1 :: Integer)
  let b2 = Sum (2 :: Integer)
  let c3 = Sum (3 :: Integer)
  let left = (a1 <> b2) <> c3
  let right = a1 <> (b2 <> c3)
  putStrLn ("Left: " ++ show left)
  putStrLn ("Right: " ++ show right)
  putStrLn ("Equal: " ++ show (left == right))