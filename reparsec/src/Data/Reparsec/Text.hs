-- |

module Data.Reparsec.Text where

import Data.Reparsec

digit :: (SomeError e, NoMoreInput e) =>  Parser [Char] e Char
digit = do
  c <- nextElement
  if isDigit c
    then pure c
    else Parser (\mi _done failed -> failed mi (someError "non-digit"))

letter :: (SomeError e, NoMoreInput e) =>  Parser [Char] e Char
letter = do
  c <- nextElement
  if isLetter c
    then pure c
    else Parser (\mi _done failed -> failed mi (someError "non-letter"))

letters :: (SomeError e, NoMoreInput e) => Parser [Char] [e] [Char]
letters = do
  c <- letter
  d <- fmap Just letters <> pure Nothing
  pure (c : fromMaybe [] d)

digits :: (NoMoreInput e, SomeError e) => Parser [Char] [e] [Char]
digits = do
  c <- digit
  d <- fmap Just digits <> pure Nothing
  pure (c : fromMaybe [] d)
