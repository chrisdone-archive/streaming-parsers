{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , More(..)
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe

--------------------------------------------------------------------------------
-- Parser type

-- | A parser. Takes as input maybe a value. Nothing terminates the
-- input. Takes two continuations: one for success and one for failure.
newtype ParserT input error m value = ParserT
  { runParserT :: forall result.
                  input
               -> Int
               -> More
               -> (input -> Int -> More -> value -> m (Result m input error result))
               -> (input -> Int -> More -> error -> m (Result m input error result))
               -> m (Result m input error result)
  }

-- | Have we read all available input?
data More
  = Complete
  | Incomplete

instance Monad m => Monad (ParserT i e m) where
  return x = ParserT (\i pos more done _failed -> done i pos more x)
  {-# INLINABLE return #-}
  m >>= f =
    ParserT
      (\i pos more done failed ->
         runParserT
           m
           i
           pos
           more
           (\i' !pos' more' v -> runParserT (f v) i' pos' more' done failed)
           failed)
  {-# INLINABLE (>>=) #-}

instance MonadTrans (ParserT i e) where
  lift m =
    ParserT
      (\i pos more done _failed -> do
         v <- m
         done i pos more v)

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
  = Done !i !Int !More !r
  | Failed !i !Int !More !e
  | Partial (Maybe i -> m (Result m i e r))

instance Semigroup e => Semigroup (ParserT i e m a) where
  left <> right =
    ParserT
      (\mi pos more done failed ->
         runParserT
           left
           mi
           pos
           more
           done
           (\mi' _pos more' e ->
              runParserT
                right
                mi'
                pos
                more'
                done
                (\mi'' pos' more'' e' -> failed mi'' pos' more'' (e <> e'))))
  {-# INLINABLE (<>) #-}

--------------------------------------------------------------------------------
-- Entry points

-- | Run the parser, terminating the input if it requests more.
parseOnlyT :: Monad m => ParserT i e m a -> i -> m (Either e a)
parseOnlyT p i =
  terminate
    (runParserT
       p
       i
       0
       Incomplete
       (\inp pos more v -> pure (Done inp pos more v))
       (\inp pos more e -> pure (Failed inp pos more e)))
  where
    terminate m = do
      r <- m
      case r of
        Partial f -> terminate (f Nothing)
        Done _ _pos _more d -> pure (Right d)
        Failed _ _pos _more e -> pure (Left e)

-- | Run the parser on the input, allowing a partial result. Use this
-- for \"streaming\" parsing.
parseResultT :: (Monad m, Monoid i) => ParserT i e m a -> Maybe i -> m (Result m i e a)
parseResultT p mi =
  runParserT
    p
    (fromMaybe mempty mi)
    0
    Incomplete
    (\inp pos more v -> pure (Done inp pos more v))
    (\inp pos more e -> pure (Failed inp pos more e))

--------------------------------------------------------------------------------
-- Combinators

-- | Fail the parser with the given error.
failWith :: e -> ParserT i e m a
failWith e = ParserT (\mi pos more _done failed -> failed mi pos more e)

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
