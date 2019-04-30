-- | Parsing from an input list.

module Data.Reparsec.List
  ( nextElement
  , endOfInput
  ) where

import Data.Reparsec

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
