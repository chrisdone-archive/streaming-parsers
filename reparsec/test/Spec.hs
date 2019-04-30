module Main where

import Data.Reparsec.List
import Data.Reparsec.List.Char
import Test.Hspec
import Data.Reparsec

data ParseError
  = EndOfInput
  | NonDigit
  | NonLetter
  | ExpectedEof
  | Errors [ParseError]
  deriving (Eq, Show)

instance NoMoreInput ParseError where noMoreInputError = EndOfInput
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
          (shouldBe (parseOurs (letter <* endOfInput) "a") (Right 'a')))
  describe
    "Erroneous input"
    (do it
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
  where
    parseOurs :: Parser [Char] ParseError a -> [Char] -> Either ParseError a
    parseOurs = parseOnly
