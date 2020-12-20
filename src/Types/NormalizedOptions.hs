{-# LANGUAGE DeriveGeneric #-}

module Types.NormalizedOptions
    ( Type(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml
import qualified Data.Ratio as Ratio

instance Yaml.ToJSON Type

data Type = Type
    { work :: Int
    , velocity :: Int
    , focus :: Ratio.Ratio Int
    , pastVelocities :: [Int]
    , simulationTrials :: Int
    , simulationSeed :: Int
    }
    deriving (Eq, Show, Generic)
