{-# LANGUAGE DeriveGeneric #-}

module Types.VerboseOutput
    ( Type(..)
    ) where

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml

instance Yaml.ToJSON Type

data Type = Type
    { options :: Options.Type
    , normalizedOptions :: NormalizedOptions.Type
    }
  deriving (Show, Eq, Generic)
