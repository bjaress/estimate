{-# LANGUAGE DeriveGeneric #-}

module Types.Estimate
    ( Type(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml

instance Yaml.ToJSON Type

data Type = Type
  { timeIntervals :: Int
  , confidence :: String
  }
  deriving (Show, Eq, Generic)
