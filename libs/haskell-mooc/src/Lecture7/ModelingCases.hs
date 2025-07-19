module Lecture7.ModelingCases where

import Data.List (sortBy)
import Data.Ord (comparing)

{-
>>> :i Ordering
type Ordering :: *
data Ordering = LT | EQ | GT
  	-- Defined in ‘GHC.Types’
instance Bounded Ordering -- Defined in ‘GHC.Enum’
instance Enum Ordering -- Defined in ‘GHC.Enum’
instance Read Ordering -- Defined in ‘GHC.Read’
instance Ord Ordering -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Monoid Ordering -- Defined in ‘GHC.Base’
instance Semigroup Ordering -- Defined in ‘GHC.Base’
instance Show Ordering -- Defined in ‘GHC.Show’

>>> :t compare
compare :: Ord a => a -> a -> Ordering

ghci> :doc comparing
comparing :: Ord a => (b -> a) -> b -> b -> Ordering
  -- Identifier defined in ‘Data.Ord’

> comparing p x y = compare (p x) (p y)

example:
>>> comparing length "hello" "hi"
GT

Useful combinator for use in conjunction with the @xxxBy@ family
of functions from "Data.List", for example:

  ... sortBy (comparing fst) ...

>>> :t sortBy
sortBy :: (a -> a -> Ordering) -> [a] -> [a]

>>> :t (sortBy (comparing name))
(sortBy (comparing name)) :: [PersonL7] -> [PersonL7]

>>> sortBy (comparing age) persons
[PersonL7 {name = "Greta", age = 60},PersonL7 {name = "Hans", age = 65},PersonL7 {name = "Fridolf", age = 73}]

-}

data PersonL7 = PersonL7
  { name :: String
  , age :: Int
  }
  deriving (Show)

data SortOrderL7 = AscendingL7 | DescendingL7
data SortFieldL7 = NameL7 | AgeL7

sortByField :: SortFieldL7 -> [PersonL7] -> [PersonL7]
sortByField NameL7 = sortBy (comparing name)
sortByField AgeL7 = sortBy (comparing age)

sortPersons :: SortFieldL7 -> SortOrderL7 -> [PersonL7] -> [PersonL7]
sortPersons field AscendingL7 ps = sortByField field ps
sortPersons field DescendingL7 ps = reverse (sortByField field ps)

persons :: [PersonL7]
persons = [PersonL7 "Fridolf" 73, PersonL7 "Greta" 60, PersonL7 "Hans" 65]

firstPerson :: PersonL7
firstPerson = head persons

lastPerson :: PersonL7
lastPerson = last persons
