{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

-- | Resumable parser with backtracking-by-default.

module Data.Reparsec
  ( parseOnlyT
  , parseResultT
  , failWith
  , UnexpectedToken(..)
  , ParserT(..)
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
newtype ParserT input error m value = ParserT
  { runParserT :: forall result.
                  Maybe input
               -> (Maybe input -> value -> m (Result m input error result))
               -> (Maybe input -> error -> m (Result m input error result))
               -> m (Result m input error result)
  }

instance Monad m => Monad (ParserT i e m) where
  return x = ParserT (\mi done _failed -> done mi x)
  {-# INLINABLE return #-}
  m >>= f =
    ParserT
      (\mi done failed ->
         runParserT m mi (\mi' v -> runParserT (f v) mi' done failed) failed)
  {-# INLINABLE (>>=) #-}

instance Monad m => Applicative (ParserT i e m) where
  (<*>) = ap
  {-# INLINABLE (<*>) #-}
  pure = return
  {-# INLINABLE pure #-}

instance Monad m => Functor (ParserT i e m) where
  fmap = liftM
  {-# INLINABLE fmap #-}

-- | Result of a parser. Maybe be partial (expecting more input).
data Result m i e r
  = Done !(Maybe i) !r
  | Failed !(Maybe i) !e
  | Partial (Maybe i -> m (Result m i e r))

instance Semigroup e => Semigroup (ParserT i e m a) where
  left <> right =
    ParserT
      (\mi done failed ->
         runParserT
           left
           mi
           done
           (\_mi e -> runParserT right mi done (\mi' e' -> failed mi' (e <> e'))))
  {-# INLINABLE (<>) #-}

--------------------------------------------------------------------------------
-- Entry points

-- | Run the parser, terminating the input if it requests more.
parseOnlyT :: Monad m => ParserT i e m a -> i -> m (Either e a)
parseOnlyT p i =
  terminate
    (runParserT
       p
       (Just i)
       (\inp v -> pure (Done inp v))
       (\inp e -> pure (Failed inp e)))
  where
    terminate m = do
      r <- m
      case r of
        Partial f -> terminate (f Nothing)
        Done _ d -> pure (Right d)
        Failed _ e -> pure (Left e)

-- | Run the parser on the input, allowing a partial result. Use this
-- for \"streaming\" parsing.
parseResultT :: Monad m => ParserT i e m a -> i -> m (Result m i e a)
parseResultT p i =
  runParserT
    p
    (Just i)
    (\inp v -> pure (Done inp v))
    (\inp e -> pure (Failed inp e))

--------------------------------------------------------------------------------
-- Combinators

-- | Fail the parser with the given error.
failWith :: e -> ParserT i e m a
failWith e = ParserT (\mi _done failed -> failed mi e)

--------------------------------------------------------------------------------
-- Classes

-- | A parser may error that there is no more input.
class NoMoreInput e where
  noMoreInputError :: e

-- | A parser expected to reach end of input, but didn't.
class ExpectedEndOfInput e where
  expectedEndOfInputError :: e

-- | A token in a parse was unexpected.
class UnexpectedToken t e where
  unexpectedToken :: t -> e
