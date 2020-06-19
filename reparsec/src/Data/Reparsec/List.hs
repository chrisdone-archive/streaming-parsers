{-# LANGUAGE BangPatterns #-}
-- | Parsing from an input list.

module Data.Reparsec.List
  ( nextElement
  , expect
  , around
  , zeroOrMoreUpTo
  , endOfInput
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
    else failWith (expectedButGot a a')

-- | Expect the end of input.
endOfInput :: (ExpectedEndOfInput e, Monad m) => ParserT [a] e m ()
endOfInput =
  ParserT
    (\mi0 pos more0 done failed ->
       let go mi more =
             case drop pos mi of
               (_:_) -> failed mi (pos + 1) more expectedEndOfInputError
               [] ->
                 case more of
                   Complete -> done mi pos more ()
                   Incomplete ->
                     pure
                       (Partial
                          (\m ->
                             case m of
                               Nothing -> go mi Complete
                               Just i -> go (mi <> i) more))
        in go mi0 more0)
{-# INLINABLE endOfInput #-}

-- | Try to extract the next element from the input.
nextElement :: (NoMoreInput e, Monad m) => ParserT [a] e m a
nextElement =
  ParserT
    (\mi0 pos more0 done failed ->
       let go mi more =
             case drop pos mi of
               (x:_) -> done mi (pos + 1) more x
               [] ->
                 case more of
                   Complete -> failed mi pos more noMoreInputError
                   Incomplete ->
                     pure
                       (Partial
                          (\m ->
                             case m of
                               Nothing -> go mi Complete
                               Just i -> go (mi <> i) more))
        in go mi0 more0)
{-# INLINABLE nextElement #-}

-- | Try to extract the next element from the input.
zeroOrMoreUpTo :: (Semigroup e, Monad m) => Int -> ParserT [a] e m b -> ParserT [a] e m [b]
zeroOrMoreUpTo maxItems elementParser = go maxItems
  where
    go 0 = pure []
    go itemsLeft = do
      result <- fmap Just elementParser <> pure Nothing
      case result of
        Nothing -> pure []
        Just element -> fmap (element :) (go (itemsLeft - 1))
{-# INLINABLE zeroOrMoreUpTo #-}
