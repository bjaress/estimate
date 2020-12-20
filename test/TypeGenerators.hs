{-# LANGUAGE RecordWildCards #-}

module TypeGenerators
    ( options
    , normalizedOptions
    , positive
    , fraction
    , positiveList
    ) where

import Test.QuickCheck
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options


options :: Gen Options.Type
options = do
    definedWork <- arbitrary
    pastVelocities <- arbitrary
    targetVelocity <- arbitrary
    focus <- arbitrary
    undefinedWork <- arbitrary
    simulationTrials <- arbitrary
    simulationSeed <- arbitrary
    verbose <- arbitrary
    return Options.Type {..}

normalizedOptions :: Gen NormalizedOptions.Type
normalizedOptions = do
    work <- positive
    velocity <- positive
    focus <- fraction
    pastVelocities <- positiveList
    simulationTrials <- positive
    simulationSeed <- arbitrary
    return NormalizedOptions.Type {..}


positive :: Gen Int
positive = max 1 . abs <$> arbitrary

positiveList :: Gen [Int]
positiveList = do
    size <- positive
    vectorOf size positive

fraction :: Gen (Ratio.Ratio Int)
fraction = do
    partA <- positive
    partB <- positive
    -- always in (0, 1]
    return $ min partA partB % max partA partB
