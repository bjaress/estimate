{-# LANGUAGE RecordWildCards #-}

module Normalization (normalize) where

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Hashable as Hashable
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))

instance Hashable.Hashable Options.Type

normalize :: Options.Type -> NormalizedOptions.Type
normalize opt@Options.Type{..} = NormalizedOptions.Type
    { work = whole $ definedWork + undefinedWork

    , velocity = Maybe.fromMaybe
        (typical $ wholes pastVelocities)
        (whole <$> targetVelocity)

    , focus = fraction $ focus

    , pastVelocities = wholes pastVelocities

    , simulationTrials = max 1 simulationTrials

    , simulationSeed = Maybe.fromMaybe
        (Hashable.hash opt {Options.verbose = False})
        simulationSeed
    }



-- coerce to positive
whole :: Integral a => a -> a
whole = max 1

wholes :: Integral a => [a] -> [a]
wholes [] = [1]
wholes xs = max 1 <$> xs

fraction :: Integral a => Ratio.Ratio a -> Ratio.Ratio a
fraction = max $ 1 % 1000

-- Rule of thumb: take the second lowest
typical :: (Num a, Ord a, Enum a) => [a] -> a
typical = (!! 1) . cycle . List.sort
