{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Parsing JSON/YAML-style structures from a SAX-style stream.

module Data.Parsax
  ( objectSink
  , valueSink
  , Event(..)
  , ObjectParser(..)
  , ValueParser (..)
  ) where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Text (Text)

-- | A SAX event, containing either a scalar, array or object with keys.
data Event
  = EventScalar !ByteString
  | EventSequenceStart
  | EventSequenceEnd
  | EventObjectStart
  | EventObjectKey !ByteString
  | EventObjectEnd
  deriving (Show, Eq)

-- | Parser of an object.
data ObjectParser a
  = Field Text (Maybe a) (ValueParser a)
  | forall b c. LiftA2 (b -> c -> a) (ObjectParser b) (ObjectParser c)
  | Pure a
  | Alternative (ObjectParser a) (ObjectParser a)

-- | Parser of a value.
data ValueParser a where
  Scalar :: (ByteString -> Either Text a) -> ValueParser a
  Object :: ObjectParser a -> ValueParser a
  List :: ValueParser a -> ValueParser [a]
  Map :: (x -> a) -> ValueParser x -> ValueParser a

-- | Run an object parser on an event stream.
objectSink :: ObjectParser a -> ConduitT Event o m a
objectSink =
  \case
    Pure a -> pure a

-- | Run an value parser on an event stream.
valueSink :: ValueParser a -> ConduitT Event o m a
valueSink = undefined
