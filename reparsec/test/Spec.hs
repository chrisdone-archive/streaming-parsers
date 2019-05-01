{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Control.Monad.Trans
import Data.Functor.Identity
import Data.Reparsec
import Data.Reparsec.List
import Data.Reparsec.List.Char
import Test.Hspec hiding (around)

data ParseError
  = EndOfInput
  | NonDigit
  | NonLetter
  | ExpectedEof
  | Errors [ParseError]
  | UnexpectedToken Char
  deriving (Eq, Show)

instance NoMoreInput ParseError where noMoreInputError = EndOfInput
instance UnexpectedToken Char ParseError where unexpectedToken = UnexpectedToken
instance NonDigitError ParseError where nonDigitError = NonDigit
instance NonLetterError ParseError where nonLetterError = NonLetter
instance ExpectedEndOfInput ParseError where expectedEndOfInputError = ExpectedEof

instance Semigroup ParseError where
  Errors xs <> Errors ys = Errors (xs <> ys)
  Errors xs <> y = Errors (xs <> [y])
  x <> Errors ys = Errors ([x] <> ys)
  x <> y = Errors [x,y]

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe
    "Empty input"
    (do it "Expected empty" (shouldBe (parseOurs endOfInput []) (Right ()))
        it "Nonexpected empty" (shouldBe (parseOurs digit []) (Left EndOfInput))
        it
          "Nonexpected empty"
          (shouldBe (parseOurs letter []) (Left EndOfInput)))
  describe
    "Successful parse"
    (do describe
          "Letters or digits"
          (do it
                "abc"
                (shouldBe (parseOurs (letters <> digits) "abc") (Right "abc"))
              it
                "123"
                (shouldBe (parseOurs (letters <> digits) "123") (Right "123")))
        it
          "With end of input"
          (shouldBe
             (parseOurs ((letters <> digits) <* endOfInput) "abc")
             (Right "abc"))
        it
          "With end of input"
          (shouldBe (parseOurs (letter <* endOfInput) "a") (Right 'a'))
        it
          "Zero or more"
          (shouldBe (parseOurs (zeroOrMore letter) "a") (Right "a"))
        it
          "Zero or more: with different following token"
          (shouldBe (parseOurs (zeroOrMore letter) "a1") (Right "a"))
        it
          "Zero or more: with different following token"
          (shouldBe
             (parseOurs (zeroOrMore (letter <* digit)) "a1c2!")
             (Right "ac"))
        it
          "Zero or more: with different following token"
          (shouldBe
             (parseOurs (zeroOrMore (letter <* digit)) "a1_2!")
             (Right "a"))
        it
          "Around"
          (shouldBe (parseOurs (around 'a' '1' (pure ())) "a1") (Right ())))
  describe
    "Erroneous input"
    (do it
          "Around fail"
          (shouldBe
             (parseOurs (around 'a' '1' (pure ())) "a2")
             (Left (UnexpectedToken '2')))
        it
          "Exclamation points"
          (shouldBe
             (parseOurs (letters <> digits) "!!!")
             (Left (Errors [NonLetter, NonDigit])))
        it
          "Exclamation points"
          (shouldBe (parseOurs letters "!!!") (Left NonLetter))
        it
          "End of input expected"
          (shouldBe
             (parseOurs ((letters <> digits) <* endOfInput) "abc!")
             (Left ExpectedEof)))
  describe
    "Partial input"
    (do it
          "Not enough input"
          (shouldBe
             (case parseOursPartial (letter *> letter) "a" of
                Partial {} -> True
                _ -> False)
             True)
        it
          "Enough input"
          (shouldBe
             (case parseOursPartial (letter *> letter) "ab" of
                Done {} -> True
                _ -> False)
             True)
        it
          "Fed input"
          (shouldBe
             (case parseOursPartial (letter *> letter) "a" of
                Done {} -> True
                Partial continue ->
                  case runIdentity (continue (Just "b")) of
                    Done {} -> True
                    _ -> False
                _ -> False)
             True)
        it
          "Failure"
          (shouldBe
             (case parseOursPartial (letter *> letter) "a2" of
                Failed {} -> True
                _ -> False)
             True))
  describe
    "Transformer"
    (it "Lift" (shouldBe (parseOurs (lift (pure ())) "") (Right ())))
  where
    parseOurs ::
         ParserT [Char] ParseError Identity a -> [Char] -> Either ParseError a
    parseOurs p i = runIdentity (parseOnlyT p i)
    parseOursPartial ::
         ParserT [Char] ParseError Identity a
      -> [Char]
      -> Result Identity [Char] ParseError a
    parseOursPartial p i = runIdentity (parseResultT p (Just i))
