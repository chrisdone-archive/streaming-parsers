{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Parsing JSON/YAML-style structures from a SAX-style stream.

module Data.Parsax
  ( objectSink
  , valueSink
  , objectReparsec
  , valueReparsec
  , Event(..)
  , ObjectParser(..)
  , ValueParser (..)
  , ParseError (..)
  ) where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Reparsec
import Data.Reparsec.List
import Data.Text (Text)

data ParseError
  = UserParseError !Text
  | NoMoreInput
  | UnexpectedEvent !Event
  | Errors [ParseError]
  | ExpectedScalarButGot !Event
  deriving (Show, Eq)

instance Semigroup ParseError where
  Errors xs <> Errors ys = Errors (xs <> ys)
  Errors xs <> y = Errors (xs <> [y])
  x <> Errors ys = Errors ([x] <> ys)
  x <> y = Errors [x,y]

instance NoMoreInput ParseError where noMoreInputError = NoMoreInput
instance UnexpectedToken Event ParseError where unexpectedToken = UnexpectedEvent

-- | A SAX event, containing either a scalar, array or object with keys.
data Event
  = EventScalar !ByteString
  | EventArrayStart
  | EventArrayEnd
  | EventObjectStart
  | EventObjectKey !ByteString
  | EventObjectEnd
  deriving (Show, Eq)

-- | Parser of an object.
data ObjectParser a
  = Field Text (Maybe a) (ValueParser a)
  | forall b c. LiftA2 (b -> c -> a) (ObjectParser b) (ObjectParser c)
  | Alternative [ObjectParser a]
  | Pure a

-- | Parser of a value.
data ValueParser a where
  Scalar :: (ByteString -> Either Text a) -> ValueParser a
  Object :: ObjectParser a -> ValueParser a
  Array :: ValueParser a -> ValueParser [a]
  FMap :: (x -> a) -> ValueParser x -> ValueParser a
  AltValue :: [ValueParser a] -> ValueParser a

-- | Run an object parser on an event stream.
objectSink :: ObjectParser a -> ConduitT Event o m a
objectSink =
  \case
    Pure a -> pure a

-- | Run an value parser on an event stream.
valueSink :: ValueParser a -> ConduitT Event o m a
valueSink = undefined

-- | Make a reparsec out of an object parser.
objectReparsec :: ObjectParser a -> Parser [Event] ParseError a
objectReparsec = undefined

-- | Make a reparsec out of an value parser.
valueReparsec :: ValueParser a -> Parser [Event] ParseError a
valueReparsec =
  \case
    AltValue xs -> foldr1 (<>) (map valueReparsec xs)
    FMap f valueParser -> fmap f (valueReparsec valueParser)
    Object objectParser ->
      around EventObjectStart EventObjectEnd (objectReparsec objectParser)
    Array valueParser ->
      around
        EventArrayStart
        EventArrayEnd
        (zeroOrMore (valueReparsec valueParser))
    Scalar parse -> do
      event <- nextElement
      case event of
        EventScalar bs ->
          case parse bs of
            Right v -> pure v
            Left err -> failWith (UserParseError err)
        els -> failWith (ExpectedScalarButGot els)
