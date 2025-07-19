module Lecture7.MonoidsValidation where

-- Validation Monoid
-- Practical example of using Monoids for validation

-- ValidationResult accumlates validation errors
data ValidationResult = ValidationResult
  { isValid :: Bool
  , errors :: [String]
  }
  deriving (Show)

-- Combine validation results - valid only if both are valid
instance Semigroup ValidationResult where
  ValidationResult v1 e1 <> ValidationResult v2 e2 =
    ValidationResult
      { isValid = v1 && v2
      , errors = e1 ++ e2
      }

-- Empty validation means no errors
instance Monoid ValidationResult where
  mempty = ValidationResult True []

-- Individual validation functions
validateLength :: String -> String -> ValidationResult
validateLength fieldName value
  | length value < 3 = ValidationResult False [fieldName ++ " must be at least 3 characters"]
  | length value > 10 = ValidationResult False [fieldName ++ " must be at most 50 characters"]
  | otherwise = ValidationResult True []

validateEmail :: String -> ValidationResult
validateEmail email
  | '@' `elem` email && '.' `elem` email = ValidationResult True []
  | otherwise = ValidationResult False ["Email format is invalid"]

validateAge :: Int -> ValidationResult
validateAge age
  | age < 18 = ValidationResult False ["Age must be at least 18"]
  | age > 120 = ValidationResult False ["Age must be at most 120"]
  | otherwise = ValidationResult True []

-- User data type for validation
data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  }
  deriving (Show)

-- Comprehensive user validation using Monoid
validateUser :: User -> ValidationResult
validateUser (User name email age) =
  mconcat
    [ validateLength "Name" name
    , validateEmail email
    , validateAge age
    ]

-- Validation example
validationExample :: IO ()
validationExample = do
  putStrLn "---- Validation Monoid Example ----"

  -- Valid user
  let validUser = User "John Doe" "john@example.com" 25
  print $ validateUser validUser

  -- Invalid user - collects all errors
  let invalidUser = User "Jo" "invalid-email" 15
  print $ validateUser invalidUser