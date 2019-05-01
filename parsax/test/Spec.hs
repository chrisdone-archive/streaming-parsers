{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.ST
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Maybe
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
       (do shouldBe
             (runST
                (runConduit
                   (CL.sourceList [] .| valueSink (Object (PureObject ())))))
             (Left NoMoreInput)))
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
                   (parseOnly (valueReparsec stackLikeGrammar) stackLikeInputs)
                   stackLikeResult)))
  describe
    "Peacemeal feeding"
    (describe
       "Object"
       (do it
             "Object"
             (shouldBe
                (parsePeacemeal (valueReparsec stackLikeGrammar) stackLikeInputs)
                stackLikeResult)))
  describe
    "Conduit"
    (describe
       "Object"
       (it
          "Object"
          (shouldBe
             (runST
                (runConduit
                   (CL.sourceList stackLikeInputs .|
                    valueSink stackLikeGrammar)))
             stackLikeResult)))
  where
    parsePeacemeal ::
         (forall s. ParserT [Event] ParseError (ST s) a)
      -> [Event]
      -> Either ParseError a
    parsePeacemeal p input =
      runST
        (let loop i = do
               result <- parseResultT p (Just (take i input))
               case result of
                 Done _ r -> pure (Right r)
                 Failed _ err -> pure (Left err)
                 Partial {} ->
                   if i > length input
                     then pure (Left NoMoreInput)
                     else loop (i + 1)
          in loop 0)
    parseOnly ::
         (forall s. ParserT [Event] ParseError (ST s) a)
      -> [Event]
      -> Either ParseError a
    parseOnly p i = runST (parseOnlyT p i)

--------------------------------------------------------------------------------
-- stack.yaml-like test data

stackLikeResult :: Either a (Int, [Either Int Int])
stackLikeResult = (Right (2 :: Int, [Left (1 :: Int), Right (666 :: Int)]))

stackLikeInputs :: [Event]
stackLikeInputs =
  [ EventObjectStart
  , EventObjectKey "x"
  , EventArrayStart
  , EventScalar "1"
  , EventObjectStart
  , EventObjectKey "location"
  , EventScalar "666"
  , EventObjectEnd
  , EventArrayEnd
  , EventObjectKey "y"
  , EventScalar "2"
  , EventObjectEnd
  ]

stackLikeGrammar :: ValueParser (Int, [Either Int Int])
stackLikeGrammar =
  Object
    ((,) <$> Field "y" (Scalar (first T.pack . readEither . S8.unpack)) <*>
     (Field
        "x"
        (Array
           (fmap Left (Scalar (first T.pack . readEither . S8.unpack)) <>
            fmap
              Right
              (Object
                 (Field
                    "location"
                    (Scalar (first T.pack . readEither . S8.unpack)))))) <>
      Field "z" (Scalar (const (pure [Left 3])))))
