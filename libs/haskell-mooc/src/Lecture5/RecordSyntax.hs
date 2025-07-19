module Lecture5.RecordSyntax where

data Person = Person String Int String String String
  deriving (Show)

people :: [Person]
people =
  [ Person "Jane Doe" 21 "Houston" "Texas" "Engineer"
  , Person "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"
  , Person "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
  ]

query :: [Person] -> [Person]
query [] = []
query ((Person namee agee townn statee professionn) : xs)
  | statee == "Finland" && professionn == "Engineer" = Person namee agee townn statee professionn : query xs
  | otherwise = query xs

data Person' = Person'
  { name :: String
  , age :: Int
  , town :: String
  , state :: String
  , profession :: String
  }
  deriving (Show)

people' :: [Person']
people' =
  [ Person' "Jane Doe" 21 "Houston" "Texas" "Engineer"
  , Person' "Maija Meikäläinen" 35 "Rovaniemi" "Finland" "Engineer"
  , Person' "Mauno Mutikainen" 27 "Turku" "Finland" "Mathematician"
  ]

query' :: [Person'] -> [Person']
query' [] = []
query' (x : xs)
  | state x == "Finland" && profession x == "Engineer" = x : query' xs
  | otherwise = query' xs
