{-

stack runhaskell ./stack.hs

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Map.Strict (Map)
import           Data.Parsax
import           Data.Parsax.Yaml
import           Data.Text (Text)

data ConfigError
  = InvalidText Scalar
  | InvalidBool Scalar
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
  (result, warnings) <- parseYamlFile configObject "stack-fake.yaml"
  print configObject
  putStrLn (show (valueParserSchema configObject))
  print result
  print warnings

configObject :: ValueParser ConfigError m Config
configObject =
  Object
    (do configResolver <- Field "resolver" textScalar
        configPackages <-
          Field
            "packages"
            (Array maxBound (plainPackageScalar <> locationPackageObject))
        configExtraDeps <- Field "extra-deps" (Array maxBound textScalar)
        configFlags <- pure mempty
        pure (Config {configFlags, configResolver, configPackages, configExtraDeps}))
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
