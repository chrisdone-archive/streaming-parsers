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
  ( valueSink
  , valueReparsec
  , enforceSchema
  , valueParserSchema
  , Schema(..)
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
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Reparsec
import           Data.Reparsec.Sequence
import           Data.Semigroup.Foldable
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Vault.ST.Strict as Vault

--------------------------------------------------------------------------------
-- Types

data ParseError
  = UserParseError !Text
  | NoMoreInput
  | UnexpectedEvent !Event
  | Errors [ParseError]
  | ExpectedScalarButGot !Event
  | ExpectedObjectKeyOrEndOfObject !Event
  | NoSuchKey
  | AltError !String
  | EmptyDocument
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
valueReparsec :: PrimMonad m => ValueParser a -> ParserT (Seq Event) ParseError m a
valueReparsec =
  \case
    PureValue a -> pure a
    AltValue xs -> foldMap1 valueReparsec xs
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
      !mappingSM <- lift (stToPrim (toMappingSM objectParser))
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
objectReparsec :: PrimMonad m => MappingSM s a -> Text -> ParserT (Seq Event) ParseError m (MappingSM s a)
objectReparsec msm textKey = do
  case M.lookup textKey (msmParsers msm) of
    Nothing ->
      pure msm
    Just xs -> do
      updateVault <- foldMap1 makeAttempt xs
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
    go (AltObject xs) = do
      xs' <- mapM go xs
      pure $ asum1 xs'
    go (Field t p) = do
      key <- lift $ EitherKey <$> Vault.newKey
      let pp = ParserPair key p
      modify' $ M.insertWith (flip (<>)) t (pp :| [])
      pure $ Alt [Ap key (pure id)]

finishObjectSM :: forall s b. MappingSM s b -> Either String b
finishObjectSM msm = runAlt go (msmAlts msm)
  where
    go :: forall a. EitherKey s a -> Either String a
    go (EitherKey key) = fromMaybe (Left "not found") $ Vault.lookup key (msmVault msm)

--------------------------------------------------------------------------------
-- Conduits

-- | Run an object parser on an event stream. Leftovers events that
-- weren't consumed, in either success or failure case.
valueSink ::
     PrimMonad m => ValueParser a -> ConduitT Event o m (Either ParseError a)
valueSink valueParser = do
  mfirst <- await
  case mfirst of
    Just event -> do
      leftover event
      start
    Nothing -> pure (Left EmptyDocument)
  where
    start =
      enforceSchema (Just (valueParserSchema valueParser)) .|
      loop (parseResultT (valueReparsec valueParser))
    loop parser = do
      mevent <- await
      result <- parser (fmap pure mevent)
      case result of
        Partial resume -> do
          loop resume
        Failed remaining pos _more errors -> do
          mapM_ leftover (Seq.drop pos remaining)
          pure (Left errors)
        Done remaining pos _more a -> do
          mapM_ leftover (Seq.drop pos remaining)
          pure (Right a)

--------------------------------------------------------------------------------
-- Schema enforcing

data Schema =
  Schema
    { schemaScalar :: Bool
    , schemaObject :: Map Text Schema
    , schemaArray :: Maybe Schema
    }

instance Semigroup Schema where
  (<>) = mappend

instance Monoid Schema where
  mappend s1 s2 =
    Schema
      { schemaScalar = schemaScalar s1 || schemaScalar s2
      , schemaObject = M.unionWith mappend (schemaObject s1) (schemaObject s2)
      , schemaArray = mappend (schemaArray s1) (schemaArray s2)
      }
  mempty =
    Schema {schemaScalar = False, schemaObject = mempty, schemaArray = mempty}

-- | Create a schema out of a value parser.
valueParserSchema :: ValueParser a -> Schema
valueParserSchema =
  \case
    Scalar {} -> mempty {schemaScalar = True}
    Object objectParser -> mempty {schemaObject = objectParserSchema objectParser}
    Array valueParser -> mempty { schemaArray = Just (valueParserSchema valueParser)}
    FMapValue _ v -> valueParserSchema v
    PureValue {} -> mempty
    AltValue choices -> foldMap valueParserSchema (NE.toList choices)

-- | Create a schema out of an object parser.
objectParserSchema :: ObjectParser a -> Map Text Schema
objectParserSchema = go
  where
    go :: ObjectParser a -> Map Text Schema
    go =
      \case
        Field key valueParser ->
          M.singleton key (valueParserSchema valueParser)
        LiftA2 _ x y -> M.unionWith (<>) (go x) (go y)
        FMapObject _ x -> go x
        PureObject {} -> mempty
        AltObject choices -> foldMap go choices

enforceSchema :: Monad m => Maybe Schema -> ConduitT Event Event m ()
enforceSchema mschema = do
  mnext <- await
  case mnext of
    Just event@(EventScalar {}) ->
      case fmap schemaScalar mschema of
        Just True -> yield event
        Just False -> error "Scalars not allowed here."
        _ -> pure ()
    Just ev@EventObjectStart {} ->
      case fmap schemaObject mschema of
        Just obj
          | not (M.null obj) -> do
            yield ev
            let loop = do
                  mnext1 <- await
                  case mnext1 of
                    Just e@EventObjectEnd -> yield e
                    Just e@(EventObjectKey key) ->
                      yield e *> enforceSchema (M.lookup key obj) *> loop
                    _ ->
                      error
                        "Expected only object end or object key here. (Ignored value)"
             in loop
        Nothing ->
          let loop = do
                mnext1 <- await
                case mnext1 of
                  Just EventObjectEnd -> pure ()
                  Just (EventObjectKey {}) -> enforceSchema Nothing *> loop
                  _ ->
                    error
                      "Expected only object end or object key here. (Ignored value)"
           in loop
        _ -> error "Objects not allowed here."
    Just ev@EventArrayStart ->
      case fmap schemaArray mschema of
        Just Nothing -> error "Arrays not allowed here."
        Just (Just schema) -> do
          yield ev
          let loop = do
                mnext1 <- await
                case mnext1 of
                  Just e@EventArrayEnd -> yield e
                  Just next -> do
                    leftover next
                    enforceSchema (Just schema)
                    loop
                  _ -> error "Expected only array end or array value here."
           in loop
        Nothing ->
          let loop = do
                mnext1 <- await
                case mnext1 of
                  Just EventArrayEnd -> pure ()
                  Just next -> do
                    leftover next
                    enforceSchema Nothing
                    loop
                  _ -> error "Expected only array end or array value here."
           in loop
    Nothing -> pure ()
    Just _ -> error ("Unexpected non-starting or scalar here: " ++ show mnext)
