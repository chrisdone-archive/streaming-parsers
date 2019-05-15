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
                   (CL.sourceList [] .|
                    valueSink defaultConfig (Object (PureObject ())))))
             (Left (EmptyDocument :: ParseError ()), mempty)))
  reparsec
  peacemeal
  conduit
  yaml
  json

json :: SpecWith ()
json =
  describe
    "Json"
    (do it
          "From file, limited warnings"
          (shouldReturn
             (parseJsonFile
                defaultConfig {configMaxKeyWarnings = 3}
                stackLikeGrammar
                "test/assets/stack.json")
             ( stackLikeResultJson
             , [ IgnoredKey "extraneous"
               , IgnoredKey "extraneous1"
               , IgnoredKey "extraneous2"
               ]))
        it
          "From file"
          (shouldReturn
             (parseJsonFile
                defaultConfig
                stackLikeGrammar
                "test/assets/stack.json")
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
                 parseJsonByteString defaultConfig stackLikeGrammar bytes)
             ( stackLikeResultJson
             , [ IgnoredKey "extraneous"
               , IgnoredKey "extraneous1"
               , IgnoredKey "extraneous2"
               , IgnoredKey "extraneous3"
               ]))
        it
          "Empty"
          (shouldReturn
             (parseJsonByteString defaultConfig (Array 1 (Scalar pure)) "")
             ( Left
                 ((TokenizeError
                     (AttoParseError
                        { errorContexts = []
                        , errorMessage = "not enough input"
                        , errorPosition =
                            Position {posLine = 1, posCol = 1, posOffset = 0}
                        })) :: JsonError ())
             , mempty)))

yaml :: SpecWith ()
yaml =
  describe
    "Yaml"
    (do it
          "From file, mapping"
          (shouldReturn
             (parseYamlFile
                defaultConfig {configMaxKeyWarnings = 3}
                (Mapping 5 (Mapping 1 boolScalar))
                "test/assets/mapping.yaml")
             ( Right
                 [("package1", [("f1", True)]), ("package2", [("f2", True)])]
             , mempty))
        it
          "From file"
          (shouldReturn
             (parseYamlFile
                defaultConfig
                stackLikeGrammar
                "test/assets/stack.yaml")
             (stackLikeResultYaml, mempty))
        it
          "From string"
          (shouldReturn
             (do bytes <- S.readFile "test/assets/stack.yaml"
                 parseYamlByteString defaultConfig stackLikeGrammar bytes)
             (stackLikeResultYaml, mempty))
        it
          "Empty"
          (shouldReturn
             (parseYamlByteString defaultConfig (Array 1 (Scalar pure)) "")
             ( Left
                 (Data.Parsax.Yaml.ParseError (EmptyDocument :: ParseError ()))
             , mempty))
        describe
          "Variables"
          (do it
                "Simple"
                (shouldReturn
                   (parseYamlFile
                      defaultConfig
                      variablesGrammar
                      "test/assets/variables.yaml")
                   ( Right
                       (map
                          TextScalar
                          ["Apple", "Beachball", "Cartoon", "Duckface", "Apple"])
                   , mempty))
              it
                "Object sized"
                (shouldReturn
                   (parseYamlFile
                      defaultConfig
                      stackLikeGrammar
                      "test/assets/stack-variables.yaml")
                   (stackLikeResultVarsYaml, mempty))))

conduit :: SpecWith ()
conduit =
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
                       valueSink defaultConfig stackLikeGrammar)))
                (stackLikeResult, pure (IgnoredKey "wibble")))
           it
             "Force object error"
             (shouldBe
                (runST
                   (runConduit
                      (CL.sourceList
                         [ EventObjectStart
                         , EventObjectKey "x"
                         , EventArrayStart
                         , EventScalar (ScientificScalar 1)
                         , EventObjectStart
                         , EventObjectKey "location"
                         , EventScalar (TextScalar "666")
                         , EventObjectEnd
                         , EventArrayEnd
                         , EventObjectKey "y"
                         , EventScalar (ScientificScalar 2)
                         , EventObjectEnd
                         ] .|
                       valueSink defaultConfig stackLikeGrammar)))
                ( Left
                    (Errors
                       [ ExpectedScalarButGot EventObjectStart
                       , UserError "Expected integer."
                       ])
                , []))
           it
             "Empty object"
             (shouldBe
                (runST
                   (runConduit
                      (CL.sourceList [EventObjectStart, EventObjectEnd] .|
                       valueSink defaultConfig (Object (pure ())))))
                (Right () :: Either (ParseError ()) (), mempty))))

peacemeal :: SpecWith ()
peacemeal =
  describe
    "Peacemeal feeding"
    (describe
       "Object"
       (do it
             "Object"
             (shouldBe
                (parsePeacemeal (valueReparsec stackLikeGrammar) stackLikeInputs)
                stackLikeResult)))

reparsec :: SpecWith ()
reparsec =
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
                      (valueReparsec (Array 1 (Scalar (const (pure 1)))))
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
                         (Array 1 (intScalar <> Scalar (const (Left "Error!")))))
                      [ EventArrayStart
                      , EventScalar (TextScalar "a")
                      , EventArrayEnd
                      ])
                   (Left
                      (Errors
                         [UserError "Expected integer.", UserError "Error!"]) :: Either (ParseError Text) [Int])))
        describe
          "Object"
          (do it
                "Object"
                (shouldBe
                   (parseOnly (valueReparsec stackLikeGrammar) stackLikeInputs)
                   stackLikeResult)
                       -- Below: Without the schema filtering, we get a less
                       -- helpful error. But it's an error nontheless.
              it
                "Object with duplicate keys"
                (shouldBe
                   (parseOnly
                      (valueReparsec stackLikeGrammar)
                      stackLikeInputsWithDuplicate)
                   (Left (Errors [ExpectedScalarButGot EventObjectStart,ExpectedObjectKeyOrEndOfObject (EventScalar (ScientificScalar 777.0))])))
                       -- Below: With the schema filtering, we get a good error
                       -- about duplicate keys.
              it
                "Object with bogus fields (schema filtered)"
                (shouldBe
                   (runST
                      (runConduit
                         (CL.sourceList (toList stackLikeInputsWithDuplicate) .|
                          valueSink defaultConfig stackLikeGrammar)))
                   (Left (BadSchema (SchemaDuplicateKey "location")), mempty))))


--------------------------------------------------------------------------------
-- Helpers

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
stackLikeResultJson = first Data.Parsax.Json.ParseError stackLikeResult

stackLikeResult :: Either (ParseError e) (Int, [Either Int Int])
stackLikeResult = (Right (2 :: Int, [Left (1 :: Int), Right (666 :: Int)]))

stackLikeResultYaml :: Either (YamlError e) (Int, [Either Int Int])
stackLikeResultYaml = first Data.Parsax.Yaml.ParseError stackLikeResult

stackLikeResultVarsYaml :: Either (YamlError e) (Int, [Either Int Int])
stackLikeResultVarsYaml = first Data.Parsax.Yaml.ParseError stackLikeResultVars

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

stackLikeInputsWithDuplicate :: Seq Event
stackLikeInputsWithDuplicate =
  [ EventObjectStart
  , EventObjectKey "x"
  , EventArrayStart
  , EventScalar (ScientificScalar 1)
  , EventObjectStart
  , EventObjectKey "location"
  , EventScalar (ScientificScalar 666)
  , EventObjectKey "location"
  , EventScalar (ScientificScalar 777)
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
    xarray = Array 10 (fmap Left int <> fmap Right loc)
    zfield = fmap (pure . Left) (Field "z" int)
    loc = Object (Field "location" int)
    int = intScalar

variablesGrammar :: ValueParser () m [Scalar]
variablesGrammar = Array 10 (Scalar pure)

intScalar :: (Bounded i, Integral i) => ValueParser Text m i
intScalar =
  Scalar
    (\case
       ScientificScalar s ->
         case toBoundedInteger s of
           Nothing -> Left "Invalid bounded integer."
           Just v -> pure v
       _ -> Left "Expected integer.")

boolScalar :: ValueParser [Char] m Bool
boolScalar =
  Scalar
    (\case
       BoolScalar bool -> pure bool
       _ -> Left "Invalid bool")
