{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Json parser based on aeson.

module Data.Parsax.Json
  ( parseJsonFile
  , parseJsonByteString
  , jsonSink
  , JsonError(..)
  , AttoParseError(..)
  , Position(..)
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Resource
import qualified Data.Aeson.Parser.Internal as Aeson
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as Atto
import qualified Data.Conduit.Binary as CB
import           Data.Functor
import           Data.Parsax
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           Prelude hiding (error, undefined)

-- Copied from aeson

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

data JsonError e
  = TokenizeError !AttoParseError
  | ParseError (ParseError e)
  | ExtraneousComma
  | MisplacedComma
  | DoubleStart
  deriving (Eq, Show)

-- | The context and message from a 'A.Fail' value.
data AttoParseError
  = AttoParseError
      { errorContexts :: [String]
      , errorMessage :: String
      , errorPosition :: Position
      }
  | DivergentParser
  deriving (Eq, Show)

-- | Convert from conduit-extra's parse error type.
fromAttoParseError :: Atto.ParseError -> AttoParseError
fromAttoParseError Atto.DivergentParser = DivergentParser
fromAttoParseError Atto.ParseError {errorContexts, errorMessage, errorPosition} =
  AttoParseError
    {errorContexts, errorMessage, errorPosition = fromPosition errorPosition}
  where
    fromPosition Atto.Position {posLine, posCol, posOffset} =
      Position {posLine, posCol, posOffset}

data Position = Position
    { posLine :: !Int
    , posCol  :: !Int
    , posOffset :: !Int
    }
    deriving (Eq, Ord, Show)

parseJsonFile ::
     Config
  -> ValueParser e (ResourceT IO) a
  -> FilePath
  -> IO (Either (JsonError e) a, Seq ParseWarning)
parseJsonFile config valueParser filePath =
  runConduitRes (CB.sourceFile filePath .| sink)
  where
    sink = do
      (tokenizeResult, parseResult) <- fuseBoth jsonSink (valueSink config valueParser)
      case tokenizeResult of
        Right () -> pure (first (first ParseError) parseResult)
        Left err -> pure (Left err, mempty)

parseJsonByteString ::
     Config
  -> ValueParser e (ResourceT IO) a
  -> ByteString
  -> IO (Either (JsonError e) a, Seq ParseWarning)
parseJsonByteString config valueParser byteString =
  runConduitRes (yield byteString .| sink)
  where
    sink = do
      (tokenizeResult, parseResult) <- fuseBoth jsonSink (valueSink config valueParser)
      case tokenizeResult of
        Right () -> pure (first (first ParseError) parseResult)
        Left err -> pure (Left err, mempty)

jsonSink :: Monad m => ConduitT ByteString Event m (Either (JsonError e) ())
jsonSink = go
  where
    go = do
      result <- Atto.sinkParserEither getValueEvent
      case result of
        Left err -> pure (Left (TokenizeError (fromAttoParseError err)))
        Right event ->
          case event of
            ValueScalar scalar -> do
              yield (EventScalar scalar)
              pure (Right ())
            ValueArrayStart -> do
              yield EventArrayStart
              let loop previous = do
                    res <- Atto.sinkParserEither getArrayEvent
                    case res of
                      Left e -> pure (Left (TokenizeError (fromAttoParseError e)))
                      Right v ->
                        case v of
                          ArrayDone ->
                            case previous of
                              ArrayComma -> pure (Left ExtraneousComma)
                              _ -> do
                                yield EventArrayEnd
                                pure (Right ())
                          ArrayComma ->
                            case previous of
                              ArrayElement {} -> loop v
                              _ -> pure (Left MisplacedComma)
                          ArrayElement -> do
                            re <- go
                            case re of
                              Right () -> loop v
                              Left {} -> pure re
                          ArrayStart -> pure (Left DoubleStart)
              loop ArrayStart
            ValueObjectStart -> do
              yield EventObjectStart
              let loop previous = do
                    res <- Atto.sinkParserEither getObjectEvent
                    case res of
                      Left e -> pure (Left (TokenizeError (fromAttoParseError e)))
                      Right v ->
                        case v of
                          ObjectDone ->
                            case previous of
                              ObjectComma -> pure (Left ExtraneousComma)
                              _ -> do
                                yield EventObjectEnd
                                pure (Right ())
                          ObjectComma ->
                            case previous of
                              ObjectKey {} -> loop v
                              _ -> pure (Left MisplacedComma)
                          ObjectKey key -> do
                            yield (EventObjectKey key)
                            re <- go
                            case re of
                              Right () -> loop v
                              Left {} -> pure re
                          ObjectStart -> pure (Left DoubleStart)
              loop ObjectStart

data ArrayToken = ArrayStart | ArrayDone | ArrayComma | ArrayElement

getArrayEvent :: Parser ArrayToken
getArrayEvent = do
  skipSpace
  w <- A.peekWord8' A.<?> "Array token: element, ',', or ']'"
  case w of
    CLOSE_SQUARE -> skip *> pure ArrayDone
    COMMA -> skip *> pure ArrayComma
    _ -> pure ArrayElement

data ObjectToken = ObjectStart | ObjectDone | ObjectComma | ObjectKey !Text

getObjectEvent :: Parser ObjectToken
getObjectEvent = do
  skipSpace
  w <- A.peekWord8' A.<?> "Object token: \"key\": .., ',', or ']'"
  case w of
    CLOSE_CURLY -> skip *> pure ObjectDone
    COMMA -> skip *> pure ObjectComma
    _ -> do
      key <- (Aeson.jstring <|> fail "Expected object key") A.<?> "Object key"
      skipSpace <*
        ((A8.char ':' <|> fail "Expected colon ':' after key") A.<?>
         "Colon after object key")
      pure (ObjectKey key)

data ValueToken = ValueScalar !Scalar | ValueObjectStart | ValueArrayStart

-- More or less copied from aeson package.
getValueEvent :: Parser ValueToken
getValueEvent = do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE ->
      skip *>
      (ValueScalar . TextScalar <$>
       (Aeson.jstring_ <|> fail "expected valid remainder of json string"))
    OPEN_CURLY -> skip *> pure ValueObjectStart
    OPEN_SQUARE -> skip *> pure ValueArrayStart
    C_f -> A.string "false" *> pure (ValueScalar (BoolScalar False))
    C_t -> A.string "true" *> pure (ValueScalar (BoolScalar True))
    C_n -> A.string "null" *> pure (ValueScalar NullScalar)
    _
      | w >= 48 && w <= 57 || w == 45 ->
        ValueScalar . ScientificScalar <$> Aeson.scientific
      | otherwise ->
        fail
          "not a valid json value, expecting true/false, null, number, string, object, array"

skip :: Parser ()
skip = void A.anyWord8

-- Copied from aeson package.
-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}
