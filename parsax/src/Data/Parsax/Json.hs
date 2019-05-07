{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Json parser based on aeson.

module Data.Parsax.Json where

import qualified Data.Aeson.Parser.Internal as Aeson
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.List as CL
import           Data.Functor
import           Data.Parsax
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

jsonSink :: Monad m => ConduitT ByteString Event m (Either Data.Conduit.Attoparsec.ParseError ())
jsonSink = go
  where
    go = do
      result <- sinkParserEither getValueEvent
      case result of
        Left err -> pure (Left err)
        Right event ->
          case event of
            EventScalar {} -> do
              yield event
              pure (Right ())
            EventArrayStart -> do
              yield event
              let loop = do
                    res <- sinkParserEither getArrayDone
                    case res of
                      Left e -> pure (Left e)
                      Right v ->
                        case v of
                          True -> do
                            yield EventArrayEnd
                            pure (Right ())
                          False -> do
                            re <- go
                            case re of
                              Right () -> loop
                              Left {} -> pure re
              loop
            EventObjectStart -> do
              yield event
              let loop = do
                    res <- sinkParserEither getObjectEvent
                    case res of
                      Left e -> pure (Left e)
                      Right v ->
                        case v of
                          ObjectDone -> do
                            yield EventObjectEnd
                            pure (Right ())
                          ObjectComma -> loop
                          ObjectKey key -> do
                            yield (EventObjectKey key)
                            re <- go
                            case re of
                              Right () -> loop
                              Left {} -> pure re
              loop

getArrayDone :: Parser Bool
getArrayDone = do
  skipSpace
  w <- A.peekWord8'
  case w of
    CLOSE_SQUARE -> skip *> pure True
    COMMA -> skip *> pure False
    _ -> pure False

data ObjectToken = ObjectDone | ObjectComma | ObjectKey Text

getObjectEvent :: Parser ObjectToken
getObjectEvent = do
  skipSpace
  w <- A.peekWord8'
  case w of
    CLOSE_CURLY -> skip *> pure ObjectDone
    COMMA -> skip *> pure ObjectComma
    _ -> do
      key <- Aeson.jstring
      skipSpace <* (A8.char ':' A.<?> "':'")
      pure (ObjectKey key)

-- More or less copied from aeson package.
getValueEvent :: Parser Event
getValueEvent = do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE -> skip *> (EventScalar . T.encodeUtf8 <$> Aeson.jstring_) -- TODO: consolidate text types.
    OPEN_CURLY -> skip *> pure EventObjectStart
    OPEN_SQUARE -> skip *> pure EventArrayStart
    C_f -> A.string "false" *> pure (EventScalar "false") -- TODO: consolidate basic scalars.
    C_t -> A.string "true" *> pure (EventScalar "true") -- TODO: consolidate basic scalars.
    C_n -> A.string "null" *> pure (EventScalar "null") -- TODO: consolidate basic scalars.
    _
      | w >= 48 && w <= 57 || w == 45 ->
        EventScalar . S8.pack . show <$> Aeson.scientific -- TODO: consolidate basic scalars.
      | otherwise -> fail "not a valid json value"

skip :: Parser ()
skip = void A.anyWord8

-- Copied from aeson package.
-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}
