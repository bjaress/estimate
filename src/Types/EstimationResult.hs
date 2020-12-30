{-# LANGUAGE DeriveGeneric #-}

module Types.EstimationResult
    ( Type(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml

import qualified Types.Estimate as Estimate
import qualified Types.Options as Options
import qualified Types.NormalizedOptions as NormalizedOptions

instance Yaml.ToJSON Type

data Type = Type
  { estimate :: Estimate.Type
  , options :: Options.Type
  , normalizedOptions :: NormalizedOptions.Type
  }
  deriving (Show, Eq, Generic)
