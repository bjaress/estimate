{-# LANGUAGE RecordWildCards #-}

module Simulation
    ( bootstrapSprints
    , simulate
    , successRate
    , countSprints
    ) where

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options

import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))


-- Simulate many trials of completing the project
simulate :: Int -> NormalizedOptions.Type -> Ratio.Rational
simulate target NormalizedOptions.Type{..} =
    successRate (<= target) $ take simulationTrials outcomes
    where
    outcomes = countSprints work <$> simulatedSprints
    simulatedSprints = (flip bootstrapSprints) adjustedVelocities <$> generators
    -- Split off separate generator for each trial, so that trials do
    -- not affect each other.  (Using a shared stream of simulated
    -- sprints would cause the duration of one trial to affect the
    -- starting point of the next.)
    generators = List.unfoldr (Just . Random.split) (Random.mkStdGen simulationSeed)
    -- Apply the fraction of effort to be spent on the project to the
    -- simulated amounts of work done in the sprints, assuming that at
    -- least some progress is made.
    adjustedVelocities = (max 1 . floor . (* focus) . fromIntegral) <$> pastVelocities


successRate :: (a -> Bool) -> [a] -> Ratio.Rational
successRate _ [] = 0
successRate pred candidates = uncurry (%) $ foldl track (0,0) candidates
    where
    track (succ, total) cand
        | pred cand = (succ+1, total+1)
        | otherwise = (succ, total+1)

-- Turn a finite list of (actual historical) sprints sizes into an
-- infinite list of (simulated) sprint sizes by sampling with
-- replacement.
bootstrapSprints :: Random.StdGen -> [a] -> [a]
bootstrapSprints _ [] = []
bootstrapSprints gen xs = (xs !!) <$> Random.randomRs (0, length xs - 1) gen


-- Count how many sprints add up to a project, given the total work in
-- the project and the amount of work in each sprint.
countSprints :: (Ord a, Num a) => a -> [a] -> Int
countSprints totalWork = length . takeWhile (< totalWork) . scanl (+) 0
