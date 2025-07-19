module Lecture7.MonoidsTreeData where

import Data.Monoid (
  Product (Product, getProduct),
  Sum (Sum, getSum),
 )

-- Tree data structure with foldable
-- Demonstrating how Monoids work with tree structures

-- Binary tree data type
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

-- Foldable instance allows us to fold over tree elements
instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node left right) = foldMap f left <> foldMap f right

-- Using Monoids with tree folding
treeSum :: Tree Int -> Int
treeSum = getSum . foldMap Sum

treeProduct :: Tree Int -> Int
treeProduct = getProduct . foldMap Product

-- Convert tree to list using list Monoid
treeToList :: Tree a -> [a]
treeToList = foldMap (: [])

treeExample :: IO ()
treeExample = do
  putStrLn "Tree with Foldable Monoid"
  let tree = Node (Leaf 3) (Node (Leaf 5) (Leaf 7))
  putStrLn $ "1. Tree Sum: " ++ show (treeSum tree)
  putStrLn $ "2. Tree Product: " ++ show (treeProduct tree)
  putStrLn $ "3. Tree to list: " ++ show (treeToList tree)
