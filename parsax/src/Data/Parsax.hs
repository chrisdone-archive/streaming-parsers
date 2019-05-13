{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , Scalar(..)
  , ObjectParser(..)
  , ValueParser(..)
  , ParseError(..)
  , ParseWarning(..)
  , SchemaError(..)
  ) where

import qualified Control.Alt.Free as Free
import           Control.Applicative
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Reparsec
import           Data.Reparsec.Sequence
import           Data.Scientific
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Validation
import qualified Data.Vault.ST.Strict as Vault

--------------------------------------------------------------------------------
-- Types

data ParseWarning
  = IgnoredKey !Text
  | LeftoverEvents (Seq Event)
  deriving (Eq, Show)

data ParseError e
  = UserError !e
  | NoMoreInput
  | UnexpectedEvent !Event
  | Errors [ParseError e]
  | ExpectedScalarButGot !Event
  | ExpectedObjectKeyOrEndOfObject !Event
  | NoSuchKey !Text
  | EmptyDocument
  | BadSchema !SchemaError
  deriving (Show, Eq)

instance Semigroup (ParseError e) where
  Errors xs <> Errors ys = Errors (xs <> ys)
  Errors xs <> y = Errors (xs <> [y])
  x <> Errors ys = Errors ([x] <> ys)
  x <> y = Errors [x,y]

instance NoMoreInput (ParseError e) where noMoreInputError = NoMoreInput
instance UnexpectedToken Event (ParseError e) where unexpectedToken = UnexpectedEvent

-- | A SAX event, containing either a scalar, array or object with keys.
data Event
  = EventScalar !Scalar
  | EventArrayStart
  | EventArrayEnd
  | EventObjectStart
  | EventObjectKey !Text
  | EventObjectEnd
  deriving (Show, Eq)

-- | A constant atomic value.
data Scalar
  = ScientificScalar !Scientific
  | TextScalar !Text
  | BoolScalar !Bool
  | NullScalar
  deriving (Show, Eq)

-- | Parser of an object.
data ObjectParser e m a where
  Field :: Text -> ValueParser e m a -> ObjectParser e m a
  LiftA2 :: (b -> c -> a) -> ObjectParser e m b -> ObjectParser e m c -> ObjectParser e m a
  FMapObject :: (x -> a) -> ObjectParser e m x -> ObjectParser e m a
  AltObject :: (NonEmpty (ObjectParser e m a)) -> ObjectParser e m a
  PureObject :: a -> ObjectParser e m a

instance Functor (ObjectParser e m) where
  fmap = FMapObject

instance Applicative (ObjectParser e m) where
  liftA2 = LiftA2
  pure = PureObject

instance Semigroup (ObjectParser e m a) where
  AltObject xs <> AltObject ys = AltObject (xs <> ys)
  AltObject xs <> y = AltObject (xs <> (y :| []))
  x <> AltObject ys = AltObject ((x :| []) <> ys)
  x <> y = AltObject (x :| [y])

-- | Parser of a value.
data ValueParser e m a where
  Scalar :: (Scalar -> Either e a) -> ValueParser e m a
  Object :: ObjectParser e m a -> ValueParser e m a
  Array :: Int -> ValueParser e m a -> ValueParser e m [a]
  FMapValue :: (x -> a) -> ValueParser e m x -> ValueParser e m a
  AltValue :: NonEmpty (ValueParser e m a) -> ValueParser e m a
  PureValue :: a -> ValueParser e m a
  CheckValue :: (i -> m (Either e a)) -> ValueParser e m i -> ValueParser e m a

instance Show (ValueParser e m a) where
  show =
    \case
      Scalar {} -> "(Scalar Func)"
      Object op -> "(Object " ++ show op ++ ")"
      Array size vp -> "(Array " ++ show size ++ " " ++ show vp ++ ")"
      FMapValue _f vp -> "(FMapValue Func " ++ show vp ++ ")"
      AltValue xs -> "(AltValue " ++ show (toList xs) ++ ")"
      PureValue _a -> "(PureValue Value)"
      CheckValue _ v -> "(CheckValue Func " ++ show v ++ ")"

instance Show (ObjectParser e m a) where
  show =
    \case
      Field key vp -> "(Field " ++ show key ++ " " ++ show vp ++ ")"
      FMapObject _f vp -> "(FMapObject Func " ++ show vp ++ ")"
      LiftA2 _f vp1 vp ->
        "(LiftA2 Func " ++ show vp1 ++ " " ++ show vp ++ ")"
      AltObject xs -> "(AltObject " ++ show (toList xs) ++ ")"
      PureObject _a -> "(PureObject Value)"

instance Functor (ValueParser e m) where
  fmap = FMapValue

instance Semigroup (ValueParser e m a) where
  AltValue xs <> AltValue ys = AltValue (xs <> ys)
  AltValue xs <> y = AltValue (xs <> (y :| []))
  x <> AltValue ys = AltValue ((x :| []) <> ys)
  x <> y = AltValue (x :| [y])

--------------------------------------------------------------------------------
-- Reparsecs

-- | Make a reparsec out of a value parser.
valueReparsec ::
     PrimMonad m
  => ValueParser e m a
  -> ParserT (Seq Event) (ParseError e) m a
valueReparsec =
  \case
    PureValue a -> pure a
    CheckValue f m -> do
      i <- valueReparsec m
      result <- lift (f i)
      case result of
        Left err -> failWith (UserError err)
        Right b -> pure b
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
        Failure err -> failWith err
        Success a -> pure a
    Array limit valueParser ->
      around
        EventArrayStart
        EventArrayEnd
        (zeroOrMoreUpTo limit (valueReparsec valueParser))
    Scalar parse -> do
      event <- nextElement
      case event of
        EventScalar bs ->
          case parse bs of
            Right v -> pure v
            Left err -> failWith (UserError err)
        els -> failWith (ExpectedScalarButGot els)

-- | Make a reparsec out of an object parser.
objectReparsec ::
     PrimMonad m
  => MappingSM s e m a
  -> Text
  -> ParserT (Seq Event) (ParseError e) m (MappingSM s e m a)
objectReparsec msm textKey = do
  case M.lookup textKey (msmParsers msm) of
    Nothing -> pure msm
    Just xs -> do
      updateVault <- foldMap1 makeAttempt xs
      pure
        msm
          { msmParsers = M.delete textKey (msmParsers msm)
          , msmVault = updateVault (msmVault msm)
          }
  where
    makeAttempt (ParserPair (EitherKey _ key) valueParser) = do
      result <- valueReparsec valueParser
      pure (Vault.insert key (Right result))

--------------------------------------------------------------------------------
-- MSM

data EitherKey s e a where
  EitherKey :: !Text -> !(Vault.Key s (Either (ParseError e) a)) -> EitherKey s e a

data ParserPair e m s where
  ParserPair :: EitherKey s e a -> ValueParser e m a -> ParserPair e m s

data MappingSM s e m a = MappingSM
  { msmAlts :: !(Free.Alt (EitherKey s e) a)
  , msmParsers :: !(Map Text (NonEmpty (ParserPair e m s)))
  , msmVault :: !(Vault.Vault s)
  }

toMappingSM :: ObjectParser e m a -> ST s (MappingSM s e m a)
toMappingSM mp = do
  (alts, parsers) <- runStateT (go mp) mempty
  pure MappingSM {msmAlts = alts, msmParsers = parsers, msmVault = Vault.empty}
  where
    go ::
         ObjectParser e m a
      -> StateT (Map Text (NonEmpty (ParserPair e m s))) (ST s) (Free.Alt (EitherKey s e) a)
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
      key <- lift $ EitherKey t <$> Vault.newKey
      let pp = ParserPair key p
      modify' $ M.insertWith (flip (<>)) t (pp :| [])
      pure $ Free.Alt (pure (toAltF key))
    toAltF x = Free.Ap x (pure id)

finishObjectSM :: forall s b e m. MappingSM s e m b -> Validation (ParseError e) b
finishObjectSM msm = Free.runAlt go (msmAlts msm)
  where
    go :: forall a. EitherKey s e a -> Validation (ParseError e) a
    go (EitherKey keyText key) =
      maybe
        (Failure (NoSuchKey keyText))
        (either Failure Success)
        (Vault.lookup key (msmVault msm))

--------------------------------------------------------------------------------
-- Conduits

-- | Run an object parser on an event stream. Leftovers events that
-- weren't consumed, in either success or failure case.
valueSink ::
     PrimMonad m
  => ValueParser e m a
  -> ConduitT Event o m (Either (ParseError e) a, Seq ParseWarning)
valueSink valueParser = do
  mfirst <- await
  case mfirst of
    Just event -> do
      leftover event
      start
    Nothing -> pure (Left EmptyDocument, mempty)
  where
    start = do

      (enforceResult, parseResult) <-
        fuseBoth
          (enforceSchema (Just (valueParserSchema valueParser)))
          (loop (parseResultT (valueReparsec valueParser)))
      pure
        (case enforceResult of
           (SchemaOK, warnings) -> second (<> warnings) parseResult
           (SchemaError err, warnings) -> (Left (BadSchema err), warnings))
    loop parser = do
      mevent <- await
      result <- lift (parser (fmap pure mevent))
      case result of
        Partial resume -> do
          loop resume
        Failed remaining pos _more errors -> do
          leftovers <- makeLeftovers pos remaining
          pure (Left errors, makeWarnings leftovers)
        Done remaining pos _more a -> do
          leftovers <- makeLeftovers pos remaining
          pure (Right a, makeWarnings leftovers)
      where
        makeLeftovers pos remaining = do
          let leftovers = Seq.drop pos remaining
          mapM_ leftover leftovers
          pure leftovers
        makeWarnings leftovers =
          if Seq.null leftovers
            then mempty
            else pure (LeftoverEvents leftovers)

--------------------------------------------------------------------------------
-- Schema enforcing

data SchemaValidation
  = SchemaOK
  | SchemaError SchemaError
  deriving (Show, Eq)

data SchemaError
  = SchemaUnexpectedScalar !Event
  | SchemaWrongEventInObjectContext !Event
  | SchemaExpectedArrayOrObject !Event
  | SchemaUnterminatedObject
  | SchemaUnterminatedArray
  | SchemaTooManyArrayElements !Int
  | SchemaArrayNotAllowed
  | SchemaObjectNotAllowed
  | SchemaDuplicateKey !Text
  deriving (Show, Eq)

-- | A schema of the shape of data that can be input.
data Schema =
  Schema
    { schemaScalar :: Any
    , schemaObject :: Maybe (Map Text Schema)
    , schemaArray :: Maybe (Schema, Max Int)
    } deriving (Show)

instance Semigroup Schema where
  (<>) = mappend

instance Monoid Schema where
  mappend s1 s2 = result
    where
      result =
        Schema
          { schemaScalar = schemaScalar s1 <> schemaScalar s2
          , schemaObject =
              case (schemaObject s1, schemaObject s2) of
                (Just o1, Just o2) -> Just (M.unionWith mappend o1 o2)
                (Just o, Nothing) -> Just o
                (Nothing, Just o) -> Just o
                (Nothing, Nothing) -> Nothing
          , schemaArray =
              case (schemaArray s1, schemaArray s2) of
                (Just o1, Just o2) -> Just (mappend o1 o2)
                (Just o, Nothing) -> Just o
                (Nothing, Just o) -> Just o
                (Nothing, Nothing) -> Nothing
          }
  mempty =
    Schema {schemaScalar = mempty, schemaObject = mempty, schemaArray = mempty}

-- | Create a schema out of a value parser.
valueParserSchema :: ValueParser e m a -> Schema
valueParserSchema =
  \case
    Scalar {} -> mempty {schemaScalar = Any True}
    Object objectParser ->
      mempty {schemaObject = Just (objectParserSchema objectParser)}
    Array limit valueParser ->
      mempty {schemaArray = Just (valueParserSchema valueParser, Max limit)}
    FMapValue _ v -> valueParserSchema v
    PureValue {} -> mempty
    AltValue choices -> foldMap valueParserSchema (NE.toList choices)
    CheckValue _f v -> valueParserSchema v

-- | Create a schema out of an object parser.
objectParserSchema :: ObjectParser e m a -> Map Text Schema
objectParserSchema = go
  where
    go :: ObjectParser e m a -> Map Text Schema
    go =
      \case
        Field key valueParser ->
          M.singleton key (valueParserSchema valueParser)
        LiftA2 _ x y -> M.unionWith (<>) (go x) (go y)
        FMapObject _ x -> go x
        PureObject {} -> mempty
        AltObject choices -> M.unionsWith (<>) (toList (fmap go choices))

-- | Enforce that the input events match the schema expected.
enforceSchema ::
     Monad m
  => Maybe Schema
  -> ConduitT Event Event m (SchemaValidation, Seq ParseWarning)
enforceSchema mschema0 = runStateC mempty (go mschema0)
  where
    go mschema = do
      mnext <- await
      case mnext of
        Just event@(EventScalar {}) ->
          case fmap schemaScalar mschema of
            Just (Any True) -> do
              yield event
              pure SchemaOK
            Just (Any False) ->
              pure (SchemaError (SchemaUnexpectedScalar event))
            _ -> pure SchemaOK
        Just ev@EventObjectStart {} ->
          case fmap schemaObject mschema of
            Just (Just obj) -> do
              yield ev
              let loop seen = do
                    mnext1 <- await
                    case mnext1 of
                      Just e@EventObjectEnd -> do
                        yield e
                        pure SchemaOK
                      Just e@(EventObjectKey key) -> do
                        yield e
                        let lookupResult = M.lookup key obj
                        case lookupResult of
                          Nothing -> lift (modify' (:|> IgnoredKey key))
                          Just {} -> pure ()
                        if Set.member key seen
                          then pure (SchemaError (SchemaDuplicateKey key))
                          else do
                            result <- go lookupResult
                            case result of
                              SchemaOK -> loop (Set.insert key seen)
                              SchemaError err -> pure (SchemaError err)
                      Just e ->
                        pure (SchemaError (SchemaWrongEventInObjectContext e))
                      Nothing -> pure (SchemaError SchemaUnterminatedObject)
               in loop mempty
            Just Nothing -> pure (SchemaError SchemaObjectNotAllowed)
            Nothing ->
              let loop = do
                    mnext1 <- await
                    case mnext1 of
                      Just EventObjectEnd -> pure SchemaOK
                      Just (EventObjectKey {}) -> do
                        result <- go Nothing
                        case result of
                          SchemaOK -> loop
                          SchemaError err -> pure (SchemaError err)
                      Just e ->
                        pure (SchemaError (SchemaWrongEventInObjectContext e))
                      Nothing -> pure (SchemaError SchemaUnterminatedObject)
               in loop
        Just ev@EventArrayStart ->
          case fmap schemaArray mschema of
            Just Nothing -> pure (SchemaError SchemaArrayNotAllowed)
            Just (Just (schema, maxElements)) -> do
              yield ev
              let loop remaining = do
                    mnext1 <- await
                    case mnext1 of
                      Nothing -> pure (SchemaError SchemaUnterminatedArray)
                      Just e@EventArrayEnd -> do
                        yield e
                        pure SchemaOK
                      Just next -> do
                        if remaining <= 0
                          then pure
                                 (SchemaError
                                    (SchemaTooManyArrayElements
                                       (getMax maxElements)))
                          else do
                            leftover next
                            result <- go (Just schema)
                            case result of
                              SchemaOK -> loop (remaining - 1)
                              SchemaError err -> pure (SchemaError err)
              loop (getMax maxElements)
            Nothing ->
              let loop = do
                    mnext1 <- await
                    case mnext1 of
                      Nothing -> pure (SchemaError SchemaUnterminatedArray)
                      Just EventArrayEnd -> pure SchemaOK
                      Just next -> do
                        leftover next
                        result <- go Nothing
                        case result of
                          SchemaOK -> loop
                          SchemaError err -> pure (SchemaError err)
               in loop
        Just event -> pure (SchemaError (SchemaExpectedArrayOrObject event))
        Nothing -> pure SchemaOK
