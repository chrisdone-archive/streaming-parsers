{-# LANGUAGE OverloadedStrings #-}

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Parsax
import           Data.Reparsec
import qualified Data.Text as T
import           Test.Hspec
import           Text.Read

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe
    "Empty stream"
    (it
       "Empty input"
       (do pending
           shouldBe
             (runConduitPure (CL.sourceList [] .| objectSink (PureObject ())))
             ()))
  describe
    "Reparsec"
    (do describe
          "Value"
          (do it
                "Value"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Scalar (const (pure ()))))
                      [EventArrayStart])
                   (Left (ExpectedScalarButGot EventArrayStart)))
              it
                "Fmap"
                (shouldBe
                   (parseOnly
                      (valueReparsec (FMapValue (+ 1) (Scalar (const (pure 1)))))
                      [EventScalar "1"])
                   (Right (2 :: Int)))
              it
                "Value"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Scalar (const (pure 1))))
                      [EventScalar "1"])
                   (Right (1 :: Int)))
              it
                "Value no input"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Scalar (const (pure (1 :: Int)))))
                      [])
                   (Left NoMoreInput))
              it
                "Value user parse error"
                (shouldBe
                   (parseOnly
                      (valueReparsec
                         (Scalar (first T.pack . readEither . S8.unpack)))
                      [EventScalar "a"])
                   (Left (UserParseError "Prelude.read: no parse") :: Either ParseError Int)))
        describe
          "Array"
          (do it
                "Array"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Array (Scalar (const (pure 1)))))
                      [EventArrayStart, EventScalar "1", EventArrayEnd])
                   (Right [1 :: Int]))
              it
                "Array error"
                (shouldBe
                   (parseOnly
                      (valueReparsec
                         (Array
                            (Scalar (first T.pack . readEither . S8.unpack) <>
                             Scalar (const (Left "")))))
                      [EventArrayStart, EventScalar "a", EventArrayEnd])
                   (Left (UnexpectedEvent (EventScalar "a")) :: Either ParseError [Int])))
        describe
          "Object"
          (do it
                "Object"
                (shouldBe
                   (parseOnly
                      (valueReparsec
                         (Object
                            ((,) <$> Field "y" (Scalar (const (pure 1))) <*>
                             (Field
                                "x"
                                (Array (Scalar (first T.pack . readEither . S8.unpack) <>
                                        Scalar (const (pure 4)))) <>
                              Field "z" (Scalar (const (pure [3])))))))
                      [ EventObjectStart
                      , EventObjectKey "x"
                      , EventArrayStart
                      , EventScalar "1"
                      , EventArrayEnd
                      , EventObjectKey "y"
                      , EventScalar "2"
                      , EventObjectEnd
                      ])
                   (Right (1 :: Int, [4 :: Int])))))
