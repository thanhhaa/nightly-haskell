module Lecture7.MonoidsCustoms where

-- Creating a Monoid for a custom data type

-- Statistics data type that tracks count, total, and maximum
data Stats = Stats
  { count :: Int
  , total :: Int
  , maxVal :: Int
  }
  deriving (Show)

-- Semigroup instance defines how to combine two Stats
instance Semigroup Stats where
  Stats c1 t1 m1 <> Stats c2 t2 m2 =
    Stats
      { count = c1 + c2 -- Add counts
      , total = t1 + t2 -- Add totals
      , maxVal = max m1 m2 -- Take maximum value
      }

-- Monoid instance provides empty/identity element
instance Monoid Stats where
  mempty = Stats 0 0 0 -- Empty statistics

-- Using our custom Stats Monoid
statsExample :: IO ()
statsExample = do
  putStrLn "---- Custom Stats Monoid ----"
  let stat1 = Stats 3 15 8
      stat2 = Stats 2 10 12
      stat3 = Stats 1 5 3

  print $ stat1 <> stat2
  print $ mconcat [stat1, stat2, stat3]