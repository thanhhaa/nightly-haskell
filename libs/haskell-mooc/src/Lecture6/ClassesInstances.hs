{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE TypeSynonymInstances #-}

module Lecture6.ClassesInstances where

data Colorr = Blackk | Whitee

instance Eq Colorr where
  Blackk == Blackk = True
  Whitee == Whitee = True
  _ == _ = False

instance Show Colorr where
  show Blackk = "Black"
  show Whitee = "White"

-------------------------------------------------------------------------------
-- Type Classes and Instances  ------------------------------------------------
--
-- Type Classes
-- A type class defines a set of functions that can be implemented for different types.
-- It's similar to interfaces in other languages but more powerful.
--
--
-------------------------------------------------------------------------------

-- Define the Speakable type class
-- This says: "Any type `a` that is an instance of `Speakable` must implement the `speak` function."
class Speakable a where
  speak :: a -> String

-- Define various animal types
newtype Dog = Dog String
newtype Cat = Cat String
newtype Bird = Bird String
newtype Human = Human String

-- Implement Speakable for different animals
instance Speakable Dog where
  speak (Dog dName) = dName ++ " says Woof!"

instance Speakable Cat where
  speak (Cat cName) = cName ++ " says Meow!"

instance Speakable Bird where
  speak (Bird bName) = bName ++ " says Tweet"

instance Speakable Human where
  speak (Human say) = "Human says: " ++ say

-- Make basic types speakable
-- return error
--   • Illegal instance declaration for ‘Speakable String’
--   (All instance types must be of the form (T t1 ... tn) where T is not a synonym.

--   Use TypeSynonymInstances if you want to disable this.)
--   • In the instance declaration for ‘Speakable String’
--     |
--     | instance Speakable String where
--     |          ^^^^^^^^^^^^^^^^
--
--   Or
--   -- Create a wrapper for humans
--   newtype Human = Human String
--
--   instance Speakable Human where
--     speak (Human name) = name ++ " says something intelligent!"

instance Speakable String where
  speak s = "Human says: " ++ s

lecture6 :: IO ()
lecture6 = do
  let dog = Dog "Rex"
  let cat = Cat "Whiskers"
  let bird = Bird "Tweety"
  let human = "Hello there!"
  let human2 = Human "Hello there!"
  putStrLn $ speak dog
  putStrLn $ speak cat
  putStrLn $ speak bird
  putStrLn $ speak human
  putStrLn $ speak human2

-- Function that works with any Speakable type
makeSpeak :: (Speakable a) => a -> String
makeSpeak x = "Listen: " ++ speak x

-- Example of type class constraints
animalConversation :: (Speakable a, Speakable b) => a -> b -> String
animalConversation animal1 animal2 =
  speak animal1 ++ " and " ++ speak animal2

-- More complex type class with multiple methods
class Animal a where
  name :: a -> String
  sound :: a -> String
  introduce :: a -> String
  -- Default implementation
  introduce animal = name animal ++ " make sound: " ++ sound animal

-- Make our animals instances of the Animal class
instance Animal Dog where
  name (Dog n) = n
  sound _ = "Woof!"

instance Animal Cat where
  name (Cat n) = n
  sound _ = "Meow!"

  -- Custom implementation overrides default
  introduce (Cat n) = "This elegant cat " ++ n ++ " says " ++ sound (Cat n)

-- Type class inheritance
class (Animal a) => Pet a where
  owner :: a -> String
  isGoodPet :: a -> Bool
  -- Default implementation
  isGoodPet _ = True

-- Pet owner data
newtype PetOwner = PetOwner String

-- Make Dog a Pet
instance Pet Dog where
  owner (Dog _) = "Dog lover"

-- Example of using inherited methods
describePet :: (Pet a) => a -> String
describePet pet =
  name pet
    ++ " owned by "
    ++ owner pet
    ++ if isGoodPet pet then " is a good pet!" else " need training."