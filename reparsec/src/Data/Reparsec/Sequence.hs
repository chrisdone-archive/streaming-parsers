{-# LANGUAGE BangPatterns #-}
-- | Parsing from an input list.

module Data.Reparsec.Sequence
  ( nextElement
  , endOfInput
  , expect
  , around
  , zeroOrMore
  ) where

import Data.Reparsec
import Data.Sequence (Seq(..))

-- | Wrap around something.
around ::
     (UnexpectedToken a1 e, NoMoreInput e, Eq a1, Monad m)
  => a1
  -> a1
  -> ParserT (Seq a1) e m a2
  -> ParserT (Seq a1) e m a2
around before after inner = expect before *> inner <* expect after

-- | Expect an element.
expect :: (UnexpectedToken a e, NoMoreInput e, Eq a, Monad m) => a -> ParserT (Seq a) e m ()
expect a = do
  a' <- nextElement
  if a == a'
    then pure ()
    else failWith (unexpectedToken a')

-- | Try to extract the next element from the input.
nextElement :: (NoMoreInput e, Monad m) => ParserT (Seq a) e m a
nextElement =
  ParserT (\mi0 done failed ->
       let go mi =
             case mi of
               Nothing -> failed Nothing noMoreInputError
               Just (x :<| xs) -> done (Just xs) x
               Just Empty -> pure (Partial go)
        in go mi0)
{-# INLINABLE nextElement #-}

-- | Expect the end of input.
endOfInput :: (ExpectedEndOfInput e, Monad m) => ParserT (Seq a) e m ()
endOfInput =
  ParserT (\mi0 done failed ->
       let go mi =
             case mi of
               Just Empty -> pure (Partial go)
               Just (_ :<| _) -> failed mi expectedEndOfInputError
               Nothing -> done Nothing ()
        in go mi0)
{-# INLINABLE endOfInput #-}

-- | Try to extract the next element from the input.
zeroOrMore :: (Semigroup e, Monad m) => ParserT (Seq a) e m b -> ParserT (Seq a) e m [b]
zeroOrMore elementParser = do
  result <- fmap Just elementParser <> pure Nothing
  case result of
    Nothing -> pure []
    Just element -> fmap (element :) (zeroOrMore elementParser)
{-# INLINABLE zeroOrMore #-}
