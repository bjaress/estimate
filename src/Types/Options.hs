{-# LANGUAGE DeriveGeneric #-}

module Types.Options
    ( Type(..)
    ) where

import GHC.Generics (Generic)
import qualified Data.Yaml as Yaml
import qualified Data.Ratio as Ratio

instance Yaml.ToJSON Type

data Type = Type
    { definedWork :: Int
    , pastVelocities :: [Int]
    , targetVelocity :: Maybe Int
    , focus :: Ratio.Ratio Int
    , undefinedWork :: Int
    , simulationTrials :: Int
    , simulationSeed :: Maybe Int
    , verbose :: Bool
    }
    deriving (Eq, Show, Generic)
