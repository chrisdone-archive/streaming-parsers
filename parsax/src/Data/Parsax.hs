{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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

import           Control.Alternative.Free
import           Control.Applicative
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Reparsec
import           Data.Reparsec.List
import           Data.Text (Text)
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
  | ExpectedObjectKeyOrEndOfObject !Event
  | NoSuchKey
  | AltError !String
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

instance Functor ObjectParser where
  fmap = FMapObject

instance Applicative ObjectParser where
  liftA2 = LiftA2
  pure = PureObject

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

instance Functor ValueParser where
  fmap = FMapValue

instance Semigroup (ValueParser a) where
  AltValue xs <> AltValue ys = AltValue (xs <> ys)
  AltValue xs <> y = AltValue (xs <> (y :| []))
  x <> AltValue ys = AltValue ((x :| []) <> ys)
  x <> y = AltValue (x :| [y])

--------------------------------------------------------------------------------
-- Reparsecs

-- | Make a reparsec out of a value parser.
valueReparsec :: PrimMonad m => ValueParser a -> ParserT [Event] ParseError m a
valueReparsec =
  \case
    PureValue a -> pure a
    AltValue (x :| xs) -> foldr (<>) (valueReparsec x) (map valueReparsec xs)
    FMapValue f valueParser -> fmap f (valueReparsec valueParser)
    Object objectParser -> do
      expect EventObjectStart
      let loop msm = do
            event <- nextElement
            case event of
              EventObjectKey key -> do
                msm' <- objectReparsec msm key
                loop msm'
              EventObjectEnd -> pure (finishObjectSM msm)
              ev -> failWith (ExpectedObjectKeyOrEndOfObject ev)
      !mappingSM <- lift (stToPrim(toMappingSM objectParser))
      result <- loop mappingSM
      case result of
        Left stringError -> failWith (AltError stringError)
        Right a -> pure a
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
objectReparsec :: PrimMonad m => MappingSM s a -> Text -> ParserT [Event] ParseError m (MappingSM s a)
objectReparsec msm textKey = do
  case M.lookup textKey (msmParsers msm) of
    Nothing ->
      pure msm
    Just (x :| xs) -> do
      updateVault <- foldr (<>) (makeAttempt x) (map makeAttempt xs)
      pure
        msm
          { msmParsers = M.delete textKey (msmParsers msm)
          , msmVault = updateVault (msmVault msm)
          }
  where
    makeAttempt (ParserPair (EitherKey key) valueParser) = do
      result <- valueReparsec valueParser
      pure (Vault.insert key (Right result))

--------------------------------------------------------------------------------
-- Conduits

-- | Run an object parser on an event stream.
objectSink :: ObjectParser a -> ConduitT Event o m a
objectSink = undefined

-- | Run an value parser on an event stream.
valueSink :: ValueParser a -> ConduitT Event o m a
valueSink = undefined

--------------------------------------------------------------------------------
-- MSM

newtype EitherKey s a = EitherKey (Vault.Key s (Either String a))

data ParserPair s where
  ParserPair :: EitherKey s a -> ValueParser a -> ParserPair s

data MappingSM s a = MappingSM
  { msmAlts :: !(Alt (EitherKey s) a)
  , msmParsers :: !(Map Text (NonEmpty (ParserPair s)))
  , msmVault :: !(Vault.Vault s)
  }

toMappingSM :: ObjectParser a -> ST s (MappingSM s a)
toMappingSM mp = do
  (alts, parsers) <- runStateT (go mp) mempty
  pure MappingSM {msmAlts = alts, msmParsers = parsers, msmVault = Vault.empty}
  where
    go ::
         ObjectParser a
      -> StateT (Map Text (NonEmpty (ParserPair s))) (ST s) (Alt (EitherKey s) a)
    go (PureObject a) = pure $ pure a
    go (FMapObject f x) = do
      x' <- go x
      pure $ fmap f x'
    go (LiftA2 f a b) = do
      a' <- go a
      b' <- go b
      pure $ liftA2 f a' b'
    go (AltObject (x :| xs)) = do
      x' <- go x
      xs' <- mapM go xs
      pure $ foldr (<|>) x' xs'
    go (Field t p) = do
      key <- lift $ EitherKey <$> Vault.newKey
      let pp = ParserPair key p
      modify' $ M.insertWith (<>) t (pp :| [])
      pure $ Alt [Ap key (pure id)]

finishObjectSM :: forall s b. MappingSM s b -> Either String b
finishObjectSM msm = runAlt go (msmAlts msm)
  where
    go :: forall a. EitherKey s a -> Either String a
    go (EitherKey key) = fromMaybe (Left "not found") $ Vault.lookup key (msmVault msm)
