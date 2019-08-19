{-# LANGUAGE BangPatterns #-}
-- | Parsing from an input list.

module Data.Reparsec.Sequence
  ( nextElement
  , lookAhead
  , expect
  , around
  , manyTill
  , parseConduit
  ) where

import           Control.Monad.Trans
import           Data.Conduit
import           Data.Reparsec
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

-- | Produce a stream of outputs from a stream of inputs.
parseConduit ::
     Monad m
  => ParserT (Seq i) e m o
  -> ConduitT i (Either e o) m ()
parseConduit parser0 = worker
  where
    worker = go (parseResultT parser0)
    go parser = do
      v <- await
      result <- lift (parser (fmap pure v))
      case result of
        Done input pos _more parse -> do
          mapM_ leftover (Seq.drop pos input)
          yield (Right parse)
          worker
        Failed remaining pos _more errors -> do
          mapM_ leftover (Seq.drop pos remaining)
          yield (Left errors)
          worker
        Partial cont ->
          go cont

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
    else failWith (expectedButGot a a')

-- | Try to extract the next element from the input.
nextElement :: (NoMoreInput e, Monad m) => ParserT (Seq a) e m a
nextElement =
  ParserT
    (\mi0 pos more0 done failed ->
       let go mi more =
             case Seq.drop pos mi of
               (x :<| _) -> done mi (pos + 1) more x
               Empty ->
                 case more of
                   Complete -> failed mi pos more noMoreInputError
                   Incomplete ->
                     pure
                       (Partial
                          (\m ->
                             case m of
                               Nothing -> go mempty Complete
                               Just i -> go (mi <> i) more))
        in go mi0 more0)
{-# INLINABLE nextElement #-}

-- | Look ahead by one token.
lookAhead :: (NoMoreInput e, Monad m) => ParserT (Seq a) e m a
lookAhead =
  ParserT
    (\mi0 pos more0 done failed ->
       runParserT
         nextElement
         mi0
         pos
         more0
         (\mi _pos more a -> done mi pos more a)
         failed)

-- | Try to extract the next element from the input.
manyTill ::
     (Eq a, Semigroup e, Monad m, NoMoreInput e)
  => Int
  -> a
  -> ParserT (Seq a) e m b
  -> ParserT (Seq a) e m [b]
manyTill maxItems endToken elementParser = go maxItems
  where
    go 0 = pure []
    go itemsLeft = do
      next <- lookAhead
      if next == endToken
        then pure []
        else do
          element <- elementParser
          fmap (element :) (go (itemsLeft - 1))
{-# INLINABLE manyTill #-}
