{-

This file demonstrates:

0. Custom errors.
1. Value parsing
2. Object parsing
3. Array parsing (length limited)
4. Mappings of arbitrary keys (length limited)
5. Custom checkers, monadic, producing user-defined warnings.
6. Backtracking.
7. Ignoring keys.

Output is:

("User warnings",fromList [DuplicatePackages ("some-directory" :| [])])

("Warnings",fromList [IgnoredKey "subdirs"])

("Result",Right (Config {configResolver = "lts-2.14", configPackages = [Plain "some-directory",Plain "https://example.com/foo/bar/baz-0.0.2.tar.gz",LocationGit (Location {locationPath = "git@github.com:commercialhaskell/stack.git", locationCommit = "6a86ee32e5b869a877151f74064572225e1a0398"}),LocationHg (Location {locationPath = "https://example.com/hg/repo", locationCommit = "da39a3ee5e6b4b0d3255bfef95601890afd80709"}),ExtraPackage (Extra {extraLocation = "vendor/binary", extraDep = True}),LocationGit (Location {locationPath = "git@github.com:yesodweb/wai", locationCommit = "2f8a8e1b771829f4a8a77c0111352ce45a14c30f"})], configExtraDeps = ["acme-missiles-0.3"], configFlags = fromList [("package-name",fromList [("flag-name",True)])]}))

-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Data.IORef
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Parsax
import           Data.Parsax.Yaml
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)

default (Text)

data ConfigError
  = InvalidText Scalar
  | InvalidBool Scalar
  deriving (Show)

data Warning =
  DuplicatePackages (NonEmpty Text)
  deriving (Show)

data Package =
  Plain Text | LocationGit Location | LocationHg Location | ExtraPackage Extra
  deriving (Show)

data Extra =
  Extra
    { extraLocation :: Text
    , extraDep :: Bool
    }
  deriving (Show)

data Location =
  Location
    { locationPath :: Text
    , locationCommit :: Text
    }
  deriving (Show)

data Config =
  Config
    { configResolver :: Text
    , configPackages :: [Package]
    , configExtraDeps :: [Text]
    , configFlags :: Map Text (Map Text Bool)
    }
  deriving (Show)

main :: IO ()
main = do
  ref <- newIORef mempty
  (result, warnings) <-
    runReaderT (parseYamlFile configObject "stack-fake.yaml") ref
  userwarnings <- readIORef ref
  print ("User warnings", userwarnings)
  print ("Warnings", warnings)
  print ("Result", result)

configObject ::
     (MonadReader (IORef (Seq Warning)) m, MonadIO m)
  => ValueParser ConfigError m Config
configObject =
  Object
    (do configResolver <- Field "resolver" textScalar
        configPackages <-
          (Field
             "packages"
             (CheckValue
                (\packages -> do
                   ref <- ask
                   liftIO
                     (modifyIORef
                        ref
                        (<> pure (DuplicatePackages (pure "some-directory"))))
                   pure (Right packages))
                (Array maxBound (plainPackageScalar <> locationPackageObject))))
        configExtraDeps <- Field "extra-deps" (Array maxBound textScalar)
        configFlags <-
          Field
            "flags"
            (fmap
               M.fromList
               (Mapping maxBound (fmap M.fromList (Mapping maxBound boolScalar))))
        pure
          (Config {configFlags, configResolver, configPackages, configExtraDeps}))
  where
    plainPackageScalar =
      Scalar
        (\case
           TextScalar text -> pure (Plain text)
           scalar -> Left (InvalidText scalar))
    locationPackageObject = Object (repo <> extra)
    extra =
      fmap
        ExtraPackage
        (Extra <$> Field "location" textScalar <*> Field "extra-dep" boolScalar)
    repo =
      Field
        "location"
        (fmap LocationGit (locationObject "git") <>
         fmap LocationHg (locationObject "hg"))
    locationObject key =
      Object (Location <$> Field key textScalar <*> Field "commit" textScalar)
    textScalar =
      Scalar
        (\case
           TextScalar text -> pure text
           scalar -> Left (InvalidText scalar))
    boolScalar =
      Scalar
        (\case
           BoolScalar bool -> pure bool
           scalar -> Left (InvalidBool scalar))
