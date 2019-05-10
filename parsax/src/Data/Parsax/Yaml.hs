{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Parse YAML using Parsax's interface.

module Data.Parsax.Yaml
  ( parseYamlByteString
  , parseYamlFile
  , yamlEventSource
  , yamlEventFileSource
  , eventConduit
  , YamlError(..)
  ) where

import           Prelude hiding (error, undefined)
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.Text as Atto
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.Conduit
import           Data.Foldable
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Parsax
import           Data.Scientific
import           Data.Sequence (Seq(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text.Encoding.Error
import qualified Text.Libyaml as Libyaml

data YamlError e
  = Utf8DecodeError !UnicodeException
  | ParseError !(ParseError e)
  | NoSuchAnchor !String
  | UnexpectedEnd
  | InvalidKeyAnchor
  | ExpectedKeyOrEndOfObject
  | DuplicateAnchor !String
  deriving (Eq, Show)

-- | Parse from a file.
parseYamlFile ::
     ValueParser e (ResourceT IO) a
  -> FilePath
  -> IO (Either (YamlError e) a, Seq ParseWarning)
parseYamlFile valueParser file =
  runConduitRes
    (do (lexResult, (parseResult, warnings)) <-
          fuseBoth (yamlEventFileSource file) (valueSink valueParser)
        case lexResult of
          Left err -> pure (Left err, warnings)
          Right () -> pure (first ParseError parseResult, warnings))

-- | Parse from a bytestring.
parseYamlByteString ::
     ValueParser e (ResourceT IO) a
  -> ByteString
  -> IO (Either (YamlError e) a, Seq ParseWarning)
parseYamlByteString valueParser byteString =
  runConduitRes
    (do (lexResult, (parseResult, warnings)) <-
          fuseBoth (yamlEventSource byteString) (valueSink valueParser)
        case lexResult of
          Left err -> pure (Left err, warnings)
          Right () -> pure (first ParseError parseResult, warnings))

-- | Produce a source of events from a string.
yamlEventSource :: (MonadResource m, MonadIO m) => ByteString -> ConduitM i Event m (Either (YamlError e) ())
yamlEventSource bs = Libyaml.decodeMarked bs .| eventConduit

-- | Produce a source of events from a string.
yamlEventFileSource :: (MonadResource m, MonadIO m) => FilePath -> ConduitM i Event m (Either (YamlError e) ())
yamlEventFileSource fp = Libyaml.decodeFileMarked fp .| eventConduit

-- | Environment for parsing.
data Env =
  Env
    { envAnchors :: !(IORef (Map Libyaml.AnchorName [Event]))
    }

-- | Convert YAML events to Parsax events.
eventConduit :: MonadIO m => ConduitT Libyaml.MarkedEvent Event m (Either (YamlError e) ())
eventConduit = do
  ref <- liftIO (newIORef mempty)
  transPipe (\m -> runReaderT m (Env ref)) value

--------------------------------------------------------------------------------
-- YAML conduits

-- | A YAML value.
value ::
     MonadIO m
  => ConduitT Libyaml.MarkedEvent Event (ReaderT Env m) (Either (YamlError e) ())
value = do
  mevent <- await
  case mevent of
    Nothing -> pure (Right ())
    Just Libyaml.MarkedEvent {yamlEvent = event} ->
      case event of
        Libyaml.EventStreamStart -> value
        Libyaml.EventStreamEnd -> pure (Right ())
        Libyaml.EventDocumentStart -> value
        Libyaml.EventDocumentEnd -> pure (Right ())
        Libyaml.EventAlias anchorName -> do
          result <- lookupAnchor anchorName
          case result of
            Nothing -> pure (Left (NoSuchAnchor anchorName))
            Just events -> do
              mapM_ yield events
              pure (Right ())
        Libyaml.EventScalar !bs !tag !style !manchor ->
          bind
            manchor
            (case T.decodeUtf8' bs of
               Left err -> pure (Left (Utf8DecodeError err))
               Right text -> do
                 yield (EventScalar (textToValue style tag text))
                 pure (Right ()))
        Libyaml.EventSequenceStart !_tag !_sequencestyle !manchor ->
          bind manchor array
        Libyaml.EventMappingStart !_tag !_mappingstyle !manchor ->
          bind manchor object
        Libyaml.EventSequenceEnd -> pure (Left UnexpectedEnd)
        Libyaml.EventMappingEnd -> pure (Left UnexpectedEnd)

-- | A YAML array.
array ::
     MonadIO m
  => ConduitT Libyaml.MarkedEvent Event (ReaderT Env m) (Either (YamlError e) ())
array = do
  yield EventArrayStart
  go
  where
    go = do
      mev <- await
      case mev of
        Nothing -> pure (Left UnexpectedEnd)
        Just marked@Libyaml.MarkedEvent {yamlEvent = ev} ->
          case ev of
            Libyaml.EventSequenceEnd -> do
              yield EventArrayEnd
              pure (Right ())
            _ -> do
              leftover marked
              result <- value
              case result of
                Left{} -> pure result
                Right{} -> go

-- | A YAML object.
object ::
     MonadIO m
  => ConduitT Libyaml.MarkedEvent Event (ReaderT Env m) (Either (YamlError e) ())
object = do
  yield EventObjectStart
  go
  where
    go = do
      mev <- await
      case mev of
        Nothing -> pure (Left UnexpectedEnd)
        Just Libyaml.MarkedEvent {yamlEvent = ev} ->
          case ev of
            Libyaml.EventMappingEnd -> do
              yield EventObjectEnd
              pure (Right ())
            Libyaml.EventAlias anchorName -> do
              result <- lookupAnchor anchorName
              case result of
                Nothing -> pure (Left (NoSuchAnchor anchorName))
                Just events ->
                  case events of
                    [eventObjectKey@EventObjectKey {}] -> do
                      yield eventObjectKey
                      res <- value
                      case res of
                        Left {} -> pure res
                        Right {} -> go
                    _ -> pure (Left InvalidKeyAnchor)
            Libyaml.EventScalar !bs !_tag !_style !mkeyanchor -> do
              case T.decodeUtf8' bs of
                Left err -> pure (Left (Utf8DecodeError err))
                Right text -> do
                  result <-
                    bind
                      mkeyanchor
                      (do yield (EventObjectKey text)
                          pure (Right ()))
                  case result of
                    Left {} -> pure result
                    Right () -> do
                      res <- value
                      case res of
                        Left {} -> pure res
                        Right {} -> go
            _ -> pure (Left ExpectedKeyOrEndOfObject)

--------------------------------------------------------------------------------
-- Anchor utilities

-- | If there's a variable to lookupAnchor, lookupAnchor all the events to that variable.
lookupAnchor ::
     (MonadIO m
     ,f ~ ReaderT Env m)
  => Libyaml.AnchorName
  -> ConduitT i Event f (Maybe [Event])
lookupAnchor var = do
  env <- lift ask
  let ref = envAnchors env
  mp <- liftIO (readIORef ref)
  pure (M.lookup var mp)

-- | If there's a variable to bind, bind all the events to that variable.
bind ::
     (MonadIO m
     ,f ~ ReaderT Env m)
  => Maybe Libyaml.AnchorName
  -> ConduitT i Event f (Either (YamlError e) a)
  -> ConduitT i Event f (Either (YamlError e) a)
bind Nothing src = src
bind (Just var) src = do
  (a, events) <- record src
  env <- lift ask
  let ref = envAnchors env
  mp <- liftIO (readIORef ref)
  case M.lookup var mp of
    Nothing -> do
      liftIO (modifyIORef ref (M.insert var events))
      pure a
    Just {} -> pure (Left (DuplicateAnchor var))

--------------------------------------------------------------------------------
-- Conduit utilities

-- | Record the output of a conduit and also yield its outputs
-- downstream.
record :: Monad m => ConduitT i o m a -> ConduitT i o m (a, [o])
record src = fuseBoth src recording
  where
    recording = go mempty
      where
        go acc = do
          m <- await
          case m of
            Nothing -> pure (toList acc)
            Just i -> do
              yield i
              let !consed = acc :|> i
              go consed

--------------------------------------------------------------------------------
-- Scalar utilities
--
-- Lifted from the yaml package.

textToValue :: Libyaml.Style -> Libyaml.Tag -> Text -> Scalar
textToValue Libyaml.SingleQuoted _ t = TextScalar t
textToValue Libyaml.DoubleQuoted _ t = TextScalar t
textToValue _ Libyaml.StrTag t = TextScalar t
textToValue Libyaml.Folded _ t = TextScalar t
textToValue _ _ t
    | t `elem` ["null", "Null", "NULL", "~", ""] = NullScalar
    | any (t `isLike`) ["y", "yes", "on", "true"] = BoolScalar True
    | any (t `isLike`) ["n", "no", "off", "false"] = BoolScalar False
    | Right x <- textToScientific t = ScientificScalar x
    | otherwise = TextScalar t
  where x `isLike` ref = x `elem` [ref, T.toUpper ref, titleCased]
          where titleCased = toUpper (T.head ref) `T.cons` T.tail ref

textToScientific :: Text -> Either String Scientific
textToScientific = Atto.parseOnly (num <* Atto.endOfInput)
  where
    num = (fromInteger <$> ("0x" *> Atto.hexadecimal))
      <|> (fromInteger <$> ("0o" *> octal))
      <|> Atto.scientific

    octal = T.foldl' step 0 <$> Atto.takeWhile1 isOctalDigit
      where
        isOctalDigit c = (c >= '0' && c <= '7')
        step a c = (a `shiftL` 3) .|. fromIntegral (ord c - 48)
