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
     (UnexpectedToken a1 e, NoMoreInput e, Eq a1)
  => a1
  -> a1
  -> Parser [a1] e a2
  -> Parser [a1] e a2
around before after inner = expect before *> inner <* expect after

-- | Expect an element.
expect :: (UnexpectedToken a e, NoMoreInput e, Eq a) => a -> Parser [a] e ()
expect a = do
  a' <- nextElement
  if a == a'
    then pure ()
    else failWith (unexpectedToken a')

-- | Try to extract the next element from the input.
nextElement :: NoMoreInput e => Parser [a] e a
nextElement =
  Parser (\mi0 done failed ->
       let go mi =
             case mi of
               Nothing -> failed Nothing noMoreInputError
               Just (x:xs) -> done (Just xs) x
               Just [] -> Partial go
        in go mi0)
{-# INLINABLE nextElement #-}

-- | Expect the end of input.
endOfInput :: ExpectedEndOfInput e => Parser [a] e ()
endOfInput =
  Parser (\mi0 done failed ->
       let go mi =
             case mi of
               Just [] -> Partial go
               Just (_:_) -> failed mi expectedEndOfInputError
               Nothing -> done Nothing ()
        in go mi0)
{-# INLINABLE endOfInput #-}

-- | Try to extract the next element from the input.
zeroOrMore :: Semigroup e => Parser [t] e a -> Parser [t] e [a]
zeroOrMore elementParser = do
  result <- fmap Just elementParser <> pure Nothing
  case result of
    Nothing -> pure []
    Just element -> fmap (element :) (zeroOrMore elementParser)
{-# INLINABLE zeroOrMore #-}
