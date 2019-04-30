{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Parsing JSON/YAML-style structures from a SAX-style stream.

module Data.Parsax
  ( objectSink
  , valueSink
  , valueReparsec
  , Event(..)
  , ObjectParser(..)
  , ValueParser (..)
  , ParseError (..)
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Reparsec
import           Data.Reparsec.List
import           Data.Text (Text)
import           Data.Vault.ST.Strict (Vault)
import qualified Data.Vault.ST.Strict as Vault

--------------------------------------------------------------------------------
-- Types

newtype Key = Key Text deriving (Eq, Ord, Show)

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
  | EventObjectKey !Text
  | EventObjectEnd
  deriving (Show, Eq)

-- | Parser of an object.
data ObjectParser a where
  Field :: Text -> ValueParser a -> ObjectParser a
  LiftA2 :: (b -> c -> a) -> ObjectParser b -> ObjectParser c -> ObjectParser a
  FMapObject :: (x -> a) -> ObjectParser x -> ObjectParser a
  AltObject :: (NonEmpty (ObjectParser a)) -> ObjectParser a
  PureObject :: a -> ObjectParser a

instance Semigroup (ObjectParser a) where
  AltObject xs <> AltObject ys = AltObject (xs <> ys)
  AltObject xs <> y = AltObject (xs <> (y :| []))
  x <> AltObject ys = AltObject ((x :| []) <> ys)
  x <> y = AltObject (x :| [y])

-- | Parser of a value.
data ValueParser a where
  Scalar :: (ByteString -> Either Text a) -> ValueParser a
  Object :: ObjectParser a -> ValueParser a
  Array :: ValueParser a -> ValueParser [a]
  FMapValue :: (x -> a) -> ValueParser x -> ValueParser a
  AltValue :: NonEmpty (ValueParser a) -> ValueParser a
  PureValue :: a -> ValueParser a

instance Semigroup (ValueParser a) where
  AltValue xs <> AltValue ys = AltValue (xs <> ys)
  AltValue xs <> y = AltValue (xs <> (y :| []))
  x <> AltValue ys = AltValue ((x :| []) <> ys)
  x <> y = AltValue (x :| [y])

--------------------------------------------------------------------------------
-- Reparsecs

-- | Make a reparsec out of a value parser.
valueReparsec :: ValueParser a -> Parser [Event] ParseError a
valueReparsec =
  \case
    PureValue a -> pure a
    AltValue (x :| xs) -> foldr (<>) (valueReparsec x) (map valueReparsec xs)
    FMapValue f valueParser -> fmap f (valueReparsec valueParser)
    Object objectParser ->
      around EventObjectStart EventObjectEnd (do objectReparsec objectParser
                                                 undefined)
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

-- | Make a reparsec out of an object parser.
objectReparsec :: ObjectParser a -> Parser [Event] ParseError (Vault s -> Vault s)
objectReparsec objectParser = do
  event <- nextElement
  case event of
    EventObjectKey key ->
      case M.lookup key keyParsers of
        Nothing ->
          error "No parser for that key... Could this be typesystemed away?"
        Just (x :| xs) -> foldr (<>) (makeAttempt x) (map makeAttempt xs)
  where
    keyParsers = objectParserMap objectParser
    makeAttempt (SomeParser key valueParser) = do
      x <- valueReparsec valueParser
      pure (Vault.insert key x)

--------------------------------------------------------------------------------
-- Object alternative

-- -- | Make a reparsec out of an object parser.
-- objectKeyParser :: Text -> ObjectParser a -> Either ParseError a
-- objectKeyParser key =
--   \case
--     PureObject a -> pure a
--     AltObject (x :| xs) -> foldr (<|>) (objectAlt x) (map objectAlt xs)
--     FMapObject f objectParser -> fmap f (objectAlt objectParser)
--     LiftA2 f objectParser1 objectParser2 ->
--       liftA2 f (objectAlt objectParser1) (objectAlt objectParser2)
--     Field key valueParser -> undefined

objectParserMap :: ObjectParser a -> Map Text (NonEmpty (SomeParser s))
objectParserMap = undefined

data SomeParser s =
  forall a. SomeParser (Vault.Key s a) (ValueParser a)

--------------------------------------------------------------------------------
-- Conduits

-- | Run an object parser on an event stream.
objectSink :: ObjectParser a -> ConduitT Event o m a
objectSink = undefined

-- | Run an value parser on an event stream.
valueSink :: ValueParser a -> ConduitT Event o m a
valueSink = undefined
