module Lecture5.AlgebraicDatatypes where

-- Algebraic? /ˌæl.dʒəˈbreɪ.ɪk/ ----------------------------------------------
-- Algebraic Data Types(ADTs)/ Kiểu dữ liệu đại số
--
-- Algebraic Data Types are a fundamental concept in functional programming
-- that allow you to define custom data structures by combining simpler types
-- in two main ways:
--    1. Sum Types (OR relationship)
--    2. Product Types (AND relationship)
--
-- References:
--
-- https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types
-- https://www.cis.upenn.edu/~sweirich/papers/yorgey-thesis.pdf
--

-- 1. Sum Types (OR relationship)
-- A trafic light can be Red OR Yellow OR Green
data TrafficLight = Red | Yellow | Green

-- Enumeration type (simple sum type)
data Color = Black | Purple | Blue

-- 2 . Product Types (AND relationship)
-- A person has a name AND age AND email
data Person = Person String Int String

-- Sum + product together:
data Shape = Circle Double | Rectangle Double Double | Point Float Float

area :: Shape -> Double
area (Point _ _) = 0
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- Practical Example: Maybe Type
{-
ghci> :i Maybe
type Maybe :: * -> *
data Maybe a = Nothing | Just a
        -- Defined in ‘GHC.Maybe’
instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
instance Semigroup a => Semigroup (Maybe a)
  -- Defined in ‘GHC.Base’
instance Foldable Maybe -- Defined in ‘Data.Foldable’
instance Traversable Maybe -- Defined in ‘Data.Traversable’
instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘GHC.Base’
instance MonadFail Maybe -- Defined in ‘Control.Monad.Fail’
instance Monad Maybe -- Defined in ‘GHC.Base’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
-}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

describeResult :: Maybe String -> String
describeResult Nothing = "No result found"
describeResult (Just value) = "Found: " ++ value

-- How to Algebraic datatypes work --------------------------------------------
--
-- References:
--
-- https://haskell.mooc.fi/part1#how-do-algebraic-datatypes-work
--