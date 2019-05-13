{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate documentation for a Parsax parser.

module Data.Parsax.Yaml.Doc where

import           Data.Generics
import           Data.List
import           Data.Ord
import           Data.Parsax
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tree

valueForest :: ValueParser e m a -> Forest String
valueForest =
  \case
    Scalar {} -> [Node "Scalar" []]
    Object obj -> [Node "Object" (sortBy (comparing gdepth) (objectForest obj))]
    Array _ x -> [Node "Array" (sortBy (comparing gdepth) (valueForest x))]
    FMapValue _ x -> valueForest x
    AltValue xs ->
      [Node "OneOf" (sortBy (comparing gdepth) (concatMap valueForest xs))]
    PureValue _ -> []
    CheckValue _ x -> valueForest x

objectForest :: ObjectParser e m a -> Forest String
objectForest =
  \case
    Field key valueParser ->
      [ Node
          ("Key " ++ show key)
          (sortBy (comparing gdepth) (valueForest valueParser))
      ]
    LiftA2 _ x y -> sortBy (comparing gdepth) (objectForest x <> objectForest y)
    FMapObject _ x -> objectForest x
    AltObject xs ->
      [Node "OneOf" (sortBy (comparing gdepth) (concatMap objectForest xs))]
    PureObject _ -> []

--------------------------------------------------------------------------------
-- Example grammar

stackLikeGrammar :: ValueParser Text m (Int, [Either Int Int])
stackLikeGrammar = Object ((,) <$> yfield <*> (xfield <> zfield))
  where
    yfield = Field "y" int
    xfield = Field "x" xarray
    xarray = Array 5 (fmap Left int <> fmap Right loc)
    zfield = fmap (pure . Left) (Field "z" int)
    loc = Object (Field "location" int)
    int = intScalar

intScalar :: (Bounded i, Integral i) => ValueParser Text m i
intScalar =
  Scalar
    (\case
       ScientificScalar s ->
         case toBoundedInteger s of
           Nothing -> Left "Invalid bounded integer."
           Just v -> pure v
       _ -> Left "Expected integer.")
