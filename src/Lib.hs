{-# LANGUAGE RecordWildCards #-}

module Lib
    ( estimate
    ) where

import qualified Types.Options as Options
import qualified Types.Estimate as Estimate
import qualified Types.EstimationResult as EstimationResult

import qualified PointEstimate
import qualified Normalization
import qualified Simulation

estimate :: Options.Type -> EstimationResult.Type
estimate options = EstimationResult.Type{..}
    where
    normalizedOptions = Normalization.normalize options
    estimate = Estimate.Type{..}
    target = PointEstimate.sprints normalizedOptions
    success = (++ "%") $ show $ floor $
        100 * (Simulation.simulate target normalizedOptions)

