module Lecture7.Monoids where

{-
----------- Monoids---------------------
A pattern that comes up surprisingly often in functional programming is the monoid
(not to be confused with a monad!). Explanations of monoids are often very mathematical,
but the idea is simple: combining things.

A monoid is a semigroup with a neutral element. A neutral element is a zero: an element
that does nothing when combined with other elements.

-- 0 is the neutral element of (+)
>>> 3 + 0
3
>>> 0 + 3
3

-- 1 is the neutral element of (*)
>>> 1 * 5
5

>>> 5 * 1
5

-- Type Class Monoid(from the module Data.Monoid) in Haskell

        class Semigroup a => Monoid a where
          -- Neutral/Identity element(Phần tử không/ phần tử đơn vị)
          mempty :: a

          -- Binary operation(deprecated in favour of <> from Semigroup)
          mappend :: a -> a -> a
          maapend = (<>)

          -- Combine a list of elements
          mconcat :: [a] -> a
          mconcat = foldr mappend mempty

-- Semigroup is a prerequisite for Monoid (just the associative operation)
        class Semigroup a where
          (<>) :: a -> a -> a

-- Laws that Monoids must satisfy:
    1. Associativity: (x <> y) <> z = x <> (y <> z)
    2. Left identity: mempty <> x = x
    3. Right identity: x <> mempty = x

-- Example

    instance Num a => Monoid (Sum a) where
      mempty = Sum 0

    instance Num a => Monoid (Product a) where
      mempty = Product 1

    instance Monoid [] where
      mempty = []

-}

import Data.Monoid (Product (..), Sum (..))

-- 1. Basic Monoid examples
-- These examples show fundamental Monoid instances

-- String Monoid - concatenation with empty string as identity
stringExample :: IO ()
stringExample = do
  putStrLn "---- String Monoid ----"
  print $ "Hello" <> " " <> "World"
  print $ mempty <> "Hello"
  print $ "Hello" <> mempty
  print $ mconcat ["A", "B", "C"]

-- Number Monoids - Sum and Product wrappers
numberExample :: IO ()
numberExample = do
  putStrLn "\n---- Number Monoid ----"
  print $ Sum (3 :: Int) <> Sum (4 :: Int)
  print $ mempty <> Sum (5 :: Int)
  print $ mconcat [Sum (1 :: Int), Sum 2, Sum 3]
  print $ Product (3 :: Int) <> Product 4
  print $ mempty <> Product (5 :: Int)

-- List Monoid - concatenation with empty list as identity
listExample :: IO ()
listExample = do
  putStrLn "\n---- List Monoid ----"
  print $ [1 :: Int, 2] <> [3, 4]
  print $ mempty <> [1 :: Int, 2]
  print $ [1 :: Int, 2] <> mempty
  print $ mconcat [[1 :: Int], [2, 3], [4]]

-- Maybe Monoid - takes first non-Nothing value
maybeExample :: IO ()
maybeExample = do
  putStrLn "\n---- Maybe Monoid ----"
  print $ Just (Sum (9 :: Int)) <> Just (Sum (6 :: Int))
  print $ Nothing <> Just (Sum (9 :: Int))
  print $ mconcat [Nothing, Just (Sum (3 :: Int))]

main :: IO ()
main = do
  stringExample
  numberExample
  listExample
  maybeExample