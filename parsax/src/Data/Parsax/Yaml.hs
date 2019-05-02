{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Parse YAML using Parsax's interface.

module Data.Parsax.Yaml
  ( parseYamlByteString
  , yamlEventSource
  ) where

import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Parsax
import qualified Text.Libyaml as Libyaml

-- | Parse from a bytestring.
parseYamlByteString :: ValueParser a -> ByteString -> IO (Either ParseError a)
parseYamlByteString valueParser byteString =
  runConduitRes (yamlEventSource byteString .| valueSink valueParser)

-- | Produce a source of events from a string.
yamlEventSource :: MonadResource m => ByteString -> ConduitM i Event m ()
yamlEventSource bs = Libyaml.decodeMarked bs .| CL.concatMap toEvent

-- | Convert a YAML event to a Parsax event.
toEvent :: Libyaml.MarkedEvent -> [Event]
toEvent (Libyaml.MarkedEvent {yamlEvent = event}) =
  case event of
    Libyaml.EventStreamStart -> []
    Libyaml.EventStreamEnd -> []
    Libyaml.EventDocumentStart -> []
    Libyaml.EventDocumentEnd -> []
    Libyaml.EventAlias {} -> []
    Libyaml.EventScalar !bs !_tag !_style !_anchor -> pure (EventScalar bs)
    Libyaml.EventSequenceStart !_tag !_sequencestyle !_anchor ->
      pure EventArrayStart
    Libyaml.EventSequenceEnd -> pure EventArrayEnd
    Libyaml.EventMappingStart !_tag !_mappingstyle !_anchor ->
      pure EventObjectStart
    Libyaml.EventMappingEnd -> pure EventObjectStart
