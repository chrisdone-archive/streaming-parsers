{-# LANGUAGE Rank2Types #-}

-- | Resumable parser with backtracking-by-default.

module Data.Reparsec
  ( parseOnly
  , Parser(..)
  , Result(..)
  -- Error handling
  , NoMoreInput(..)
  , ExpectedEndOfInput(..)
  ) where

import Control.Monad

--------------------------------------------------------------------------------
-- Parser type

-- | A parser. Takes as input maybe a value. Nothing terminates the
-- input. Takes two continuations: one for success and one for failure.
newtype Parser input error value = Parser
  { runParser :: forall result.
                 Maybe input
              -> (Maybe input -> value -> Result input error result)
              -> (Maybe input -> error -> Result input error result)
              -> Result input error result
  }

-- | Result of a parser. Maybe be partial (expecting more input).
data Result i e r
  = Done !(Maybe i) !r
  | Failed !(Maybe i) !e
  | Partial (Maybe i -> Result i e r)

instance Monad (Parser i e) where
  return x = Parser (\mi done _failed -> done mi x)
  {-# INLINABLE return #-}
  m >>= f =
    Parser
      (\mi done failed ->
         runParser m mi (\mi' v -> runParser (f v) mi' done failed) failed)
  {-# INLINABLE (>>=) #-}

instance Semigroup e => Semigroup (Parser i e a) where
  left <> right =
    Parser
      (\mi done failed ->
         runParser
           left
           mi
           done
           (\_mi e -> runParser right mi done (\mi' e' -> failed mi' (e <> e'))))
  {-# INLINABLE (<>) #-}

instance Applicative (Parser i e) where
  (<*>) = ap
  {-# INLINABLE (<*>) #-}
  pure = return
  {-# INLINABLE pure #-}

instance Functor (Parser i e) where
  fmap = liftM
  {-# INLINABLE fmap #-}

--------------------------------------------------------------------------------
-- API

parseOnly :: Parser i e a -> i -> Either e a
parseOnly p i =
  terminate (runParser p (Just i) Done Failed)
  where
    terminate r =
      case r of
        Partial f -> terminate (f Nothing)
        Done _ d -> Right d
        Failed _ e -> Left e

-- | A parser may error that there is no more input.
class NoMoreInput e where
  noMoreInputError :: e

-- | A parser expected to reach end of input, but didn't.
class ExpectedEndOfInput e where
  expectedEndOfInputError :: e
