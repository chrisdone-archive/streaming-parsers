{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Parse YAML using Parsax's interface.

module Data.Parsax.Yaml
  ( parseYamlByteString
  , parseYamlFile
  , yamlEventSource
  , yamlEventFileSource
  ) where

import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Parsax
import qualified Data.Text.Encoding as T
import qualified Text.Libyaml as Libyaml

-- | Parse from a file.
parseYamlFile :: ValueParser a -> FilePath -> IO (Either ParseError a)
parseYamlFile valueParser file =
  runConduitRes (yamlEventFileSource file .| valueSink valueParser)

-- | Parse from a bytestring.
parseYamlByteString :: ValueParser a -> ByteString -> IO (Either ParseError a)
parseYamlByteString valueParser byteString =
  runConduitRes (yamlEventSource byteString .| valueSink valueParser)

-- | Produce a source of events from a string.
yamlEventSource :: MonadResource m => ByteString -> ConduitM i Event m ()
yamlEventSource bs = Libyaml.decodeMarked bs .| eventConduit

-- | Produce a source of events from a string.
yamlEventFileSource :: MonadResource m => FilePath -> ConduitM i Event m ()
yamlEventFileSource fp = Libyaml.decodeFileMarked fp .| eventConduit

-- | Convert a YAML event to a Parsax event.
eventConduit :: Monad m => ConduitT Libyaml.MarkedEvent Event m ()
eventConduit = value
  where
    value = do
      mevent <- await
      case mevent of
        Nothing -> pure ()
        Just Libyaml.MarkedEvent {yamlEvent = event} ->
          case event of
            Libyaml.EventStreamStart -> value
            Libyaml.EventStreamEnd -> pure ()
            Libyaml.EventDocumentStart -> value
            Libyaml.EventDocumentEnd -> pure ()
            Libyaml.EventAlias {} -> value
            Libyaml.EventScalar !bs !_tag !_style !_anchor -> do
              yield (EventScalar bs)
            Libyaml.EventSequenceStart !_tag !_sequencestyle !_anchor -> do
              let go = do
                    mev <- await
                    case mev of
                      Nothing -> error "Expected end of sequence."
                      Just marked@Libyaml.MarkedEvent {yamlEvent = ev} ->
                        case ev of
                          Libyaml.EventSequenceEnd -> do
                            yield EventArrayEnd
                          _ -> do
                            leftover marked
                            value
                            go
              yield EventArrayStart
              go
            Libyaml.EventMappingStart !_tag !_mappingstyle !_anchor -> do
              let go = do
                    mev <- await
                    case mev of
                      Nothing -> error "Expected end of mapping."
                      Just Libyaml.MarkedEvent {yamlEvent = ev} ->
                        case ev of
                          Libyaml.EventMappingEnd -> do
                            yield EventObjectEnd
                          Libyaml.EventScalar !bs !_tag !_style !_anchor -> do
                            yield (EventObjectKey (T.decodeUtf8 bs))
                            value
                            go
                          _ -> error "Expected key or end of object."
              yield EventObjectStart
              go
            Libyaml.EventSequenceEnd -> error "Unexpected mapping end."
            Libyaml.EventMappingEnd -> error "Unexpected mapping end."
