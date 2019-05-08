{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.ST
import           Data.Bifunctor
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Data.Parsax
import           Data.Parsax.Json
import           Data.Parsax.Yaml
import           Data.Reparsec
import           Data.Scientific
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Test.Hspec

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
             (Left (EmptyDocument :: ParseError ()), mempty)))
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
                      [EventScalar (TextScalar "1")])
                   (Right (2 :: Int)))
              it
                "Value"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Scalar (const (pure 1))))
                      [EventScalar (TextScalar "1")])
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
                      (valueReparsec intScalar)
                      [EventScalar (TextScalar "a")])
                   (Left (UserError "Expected integer.") :: Either (ParseError Text) Int)))
        describe
          "Array"
          (do it
                "Array"
                (shouldBe
                   (parseOnly
                      (valueReparsec (Array (Scalar (const (pure 1)))))
                      [ EventArrayStart
                      , EventScalar (TextScalar "1")
                      , EventArrayEnd
                      ])
                   (Right [1 :: Int]))
              it
                "Array error"
                (shouldBe
                   (parseOnly
                      (valueReparsec
                         (Array (intScalar <> Scalar (const (Left "")))))
                      [ EventArrayStart
                      , EventScalar (TextScalar "a")
                      , EventArrayEnd
                      ])
                   (Left (UnexpectedEvent (EventScalar (TextScalar "a"))) :: Either (ParseError Text) [Int])))
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
       (do it
             "Object with bogus fields"
             (shouldBe
                (runST
                   (runConduit
                      (CL.sourceList (toList stackLikeInputsWithBogusFields) .|
                       valueSink stackLikeGrammar)))
                (stackLikeResult, pure (IgnoredKey "wibble")))
           it
             "Empty object"
             (shouldBe
                (runST
                   (runConduit
                      (CL.sourceList [EventObjectStart, EventObjectEnd] .|
                       valueSink (Object (pure ())))))
                (Right () :: Either (ParseError ()) (), mempty))))
  describe
    "Yaml"
    (do it
          "From file"
          (shouldReturn
             (parseYamlFile stackLikeGrammar "test/assets/stack.yaml")
             (stackLikeResult, mempty))
        it
          "From string"
          (shouldReturn
             (do bytes <- S.readFile "test/assets/stack.yaml"
                 parseYamlByteString stackLikeGrammar bytes)
             (stackLikeResult, mempty))
        it
          "Empty"
          (shouldReturn
             (parseYamlByteString (Array (Scalar pure)) "")
             (Left (EmptyDocument :: ParseError ()), mempty))
        describe
          "Variables"
          (do it
                "Simple"
                (shouldReturn
                   (parseYamlFile variablesGrammar "test/assets/variables.yaml")
                   ( Right
                       (map
                          TextScalar
                          ["Apple", "Beachball", "Cartoon", "Duckface", "Apple"])
                   , mempty))
              it
                "Object sized"
                (shouldReturn
                   (parseYamlFile
                      stackLikeGrammar
                      "test/assets/stack-variables.yaml")
                   (stackLikeResultVars, mempty))))
  describe
    "Json"
    (do it
          "From file"
          (shouldReturn
             (parseJsonFile stackLikeGrammar "test/assets/stack.json")
             ( stackLikeResultJson
             , [ IgnoredKey "extraneous"
               , IgnoredKey "extraneous1"
               , IgnoredKey "extraneous2"
               , IgnoredKey "extraneous3"
               ]))
        it
          "From string"
          (shouldReturn
             (do bytes <- S.readFile "test/assets/stack.json"
                 parseJsonByteString stackLikeGrammar bytes)
             ( stackLikeResultJson
             , [ IgnoredKey "extraneous"
               , IgnoredKey "extraneous1"
               , IgnoredKey "extraneous2"
               , IgnoredKey "extraneous3"
               ]))
        it
          "Empty"
          (shouldReturn
             (parseJsonByteString (Array (Scalar pure)) "")
             ( Left
                 ((TokenizeError
                     (AttoParseError
                        { errorContexts = []
                        , errorMessage = "not enough input"
                        , errorPosition =
                            Position {posLine = 1, posCol = 1, posOffset = 0}
                        })) :: JsonError ())
             , mempty)))
  where
    parsePeacemeal ::
         (forall s. ParserT (Seq Event) (ParseError Text) (ST s) a)
      -> Seq Event
      -> Either (ParseError Text) a
    parsePeacemeal p input =
      runST
        (let loop i = do
               result <- parseResultT p (Just (Seq.take i input))
               case result of
                 Done _ _ _ r -> pure (Right r)
                 Failed _ _ _ err -> pure (Left err)
                 Partial {} ->
                   if i > length input
                     then pure (Left NoMoreInput)
                     else loop (i + 1)
          in loop 0)
    parseOnly ::
         (forall s. ParserT (Seq Event) (ParseError Text) (ST s) a)
      -> Seq Event
      -> Either (ParseError Text) a
    parseOnly p i = runST (parseOnlyT p i)

--------------------------------------------------------------------------------
-- stack.yaml-like test data

stackLikeResultJson :: Either (JsonError e) (Int, [Either Int Int])
stackLikeResultJson = first ParsaxError stackLikeResult

stackLikeResult :: Either (ParseError e) (Int, [Either Int Int])
stackLikeResult = (Right (2 :: Int, [Left (1 :: Int), Right (666 :: Int)]))

stackLikeResultVars :: Either (ParseError e) (Int, [Either Int Int])
stackLikeResultVars =
  (Right
     ( 2 :: Int
     , [ Left (1 :: Int)
       , Right (666 :: Int)
       , Right (666 :: Int)
       , Right 777
       , Right 888
       ]))

stackLikeInputsWithBogusFields :: Seq Event
stackLikeInputsWithBogusFields =
  [EventObjectStart, EventObjectKey "wibble"] <> stackLikeInputs <>
  [ EventObjectKey "x"
  , EventArrayStart
  , EventScalar (ScientificScalar 1)
  , EventObjectStart
  , EventObjectKey "location"
  , EventScalar (ScientificScalar 666)
  , EventObjectEnd
  , EventArrayEnd
  , EventObjectKey "y"
  , EventScalar (ScientificScalar 2)
  , EventObjectEnd
  ]

stackLikeInputs :: Seq Event
stackLikeInputs =
  [ EventObjectStart
  , EventObjectKey "x"
  , EventArrayStart
  , EventScalar (ScientificScalar 1)
  , EventObjectStart
  , EventObjectKey "location"
  , EventScalar (ScientificScalar 666)
  , EventObjectEnd
  , EventArrayEnd
  , EventObjectKey "y"
  , EventScalar (ScientificScalar 2)
  , EventObjectEnd
  ]

stackLikeGrammar :: ValueParser Text m (Int, [Either Int Int])
stackLikeGrammar = Object ((,) <$> yfield <*> (xfield <> zfield))
  where
    yfield = Field "y" int
    xfield = Field "x" xarray
    xarray = Array (fmap Left int <> fmap Right loc)
    zfield = fmap (pure . Left) (Field "z" int)
    loc = Object (Field "location" int)
    int = intScalar

variablesGrammar :: ValueParser () m [Scalar]
variablesGrammar = Array (Scalar pure)

intScalar :: (Bounded i, Integral i) => ValueParser Text m i
intScalar =
  Scalar
    (\case
       ScientificScalar s ->
         case toBoundedInteger s of
           Nothing -> Left "Invalid bounded integer."
           Just v -> pure v
       _ -> Left "Expected integer.")
