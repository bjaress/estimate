{-# LANGUAGE RecordWildCards #-}

module PointEstimate
    ( sprints
    ) where

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options

sprints :: NormalizedOptions.Type -> Int
sprints NormalizedOptions.Type{..} =
    ceiling $ fromIntegral work / (focus * fromIntegral velocity)
