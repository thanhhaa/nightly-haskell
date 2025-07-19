module Data.ZAuth.Token where

-- https://github.com/wireapp/wire-server/commit/b69e5f3cacbf7521e0fb12e8b9bbf2da6916cd19#diff-35d43f65e21356c3ec7ae9b91bf24bb6ed874e319f6e9555b2c7ecf7c4d7128c
data Type = A | U | B | P
  deriving (Eq, Show)

-- data Token a = Token
--   { _singature :: !Sing

--   }