{-# LANGUAGE BangPatterns #-}
-- | Parsing from an input list.

module Data.Reparsec.List
  ( nextElement
  , endOfInput
  , expect
  , around
  , zeroOrMore
  ) where

import Data.Reparsec

-- | Wrap around something.
around ::
     (UnexpectedToken a1 e, NoMoreInput e, Eq a1, Monad m)
  => a1
  -> a1
  -> ParserT [a1] e m a2
  -> ParserT [a1] e m a2
around before after inner = expect before *> inner <* expect after

-- | Expect an element.
expect :: (UnexpectedToken a e, NoMoreInput e, Eq a, Monad m) => a -> ParserT [a] e m ()
expect a = do
  a' <- nextElement
  if a == a'
    then pure ()
    else failWith (unexpectedToken a')

-- | Try to extract the next element from the input.
nextElement :: (NoMoreInput e, Monad m) => ParserT [a] e m a
nextElement =
  ParserT (\mi0 done failed ->
       let go mi =
             case mi of
               Nothing -> failed Nothing noMoreInputError
               Just (x:xs) -> done (Just xs) x
               Just [] -> pure (Partial go)
        in go mi0)
{-# INLINABLE nextElement #-}

-- | Expect the end of input.
endOfInput :: (ExpectedEndOfInput e, Monad m) => ParserT [a] e m ()
endOfInput =
  ParserT (\mi0 done failed ->
       let go mi =
             case mi of
               Just [] -> pure (Partial go)
               Just (_:_) -> failed mi expectedEndOfInputError
               Nothing -> done Nothing ()
        in go mi0)
{-# INLINABLE endOfInput #-}

-- | Try to extract the next element from the input.
zeroOrMore :: (Semigroup e, Monad m) => ParserT [t] e m a -> ParserT [t] e m [a]
zeroOrMore elementParser = do
  result <- fmap Just elementParser <> pure Nothing
  case result of
    Nothing -> pure []
    Just element -> fmap (element :) (zeroOrMore elementParser)
{-# INLINABLE zeroOrMore #-}
