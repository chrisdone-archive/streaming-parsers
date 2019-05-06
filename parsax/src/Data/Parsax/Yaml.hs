{-# LANGUAGE TypeFamilies #-}
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

import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Foldable
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Parsax
import           Data.Sequence (Seq(..))
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
yamlEventSource :: (MonadResource m, MonadIO m) => ByteString -> ConduitM i Event m ()
yamlEventSource bs = Libyaml.decodeMarked bs .| eventConduit

-- | Produce a source of events from a string.
yamlEventFileSource :: (MonadResource m, MonadIO m) => FilePath -> ConduitM i Event m ()
yamlEventFileSource fp = Libyaml.decodeFileMarked fp .| eventConduit

-- | Convert YAML events to Parsax events.
eventConduit :: MonadIO m => ConduitT Libyaml.MarkedEvent Event m ()
eventConduit = do
  ref <- liftIO (newIORef mempty)
  transPipe (\m -> runReaderT m ref) value
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
            Libyaml.EventScalar !bs !_tag !_style !manchor ->
              bind manchor (yield (EventScalar bs))
            Libyaml.EventSequenceStart !_tag !_sequencestyle !manchor ->
              bind
                manchor
                (do let go = do
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
                    go)
            Libyaml.EventMappingStart !_tag !_mappingstyle !manchor ->
              bind
                manchor
                (do let go = do
                          mev <- await
                          case mev of
                            Nothing -> error "Expected end of mapping."
                            Just Libyaml.MarkedEvent {yamlEvent = ev} ->
                              case ev of
                                Libyaml.EventMappingEnd -> do
                                  yield EventObjectEnd
                                Libyaml.EventScalar !bs !_tag !_style !mkeyanchor -> do
                                  bind
                                    mkeyanchor
                                    (yield (EventObjectKey (T.decodeUtf8 bs)))
                                  value
                                  go
                                _ -> error "Expected key or end of object."
                    yield EventObjectStart
                    go)
            Libyaml.EventSequenceEnd -> error "Unexpected mapping end."
            Libyaml.EventMappingEnd -> error "Unexpected mapping end."

-- | If there's a variable to bind, bind all the events to that variable.
bind ::
     (MonadIO m
     ,f ~ ReaderT (IORef (Map Libyaml.AnchorName [o])) m)
  => Maybe Libyaml.AnchorName
  -> ConduitT i o f ()
  -> ConduitT i o f ()
bind Nothing src = src
bind (Just var) src = do
  events <- record src
  ref <- lift ask
  liftIO (modifyIORef ref (M.insert var events))

-- | Record the output of a conduit and also yield its outputs
-- downstream.
record :: Monad m => ConduitT i o m () -> ConduitT i o m [o]
record src = src .| recording
  where
    recording = go mempty
      where
        go acc = do
          m <- await
          case m of
            Nothing -> pure (toList acc)
            Just i -> do
              yield i
              go (acc :|> i)
