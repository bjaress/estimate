{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( main
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Options.Applicative.Help.Pretty as Pretty
import qualified Options.Applicative.Help.Chunk as Chunk
import Options.Applicative.Help.Chunk ((<</>>))
import GHC.Generics (Generic)

import qualified Data.Yaml as Yaml
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.ByteString as ByteString
import qualified Control.Monad as Monad
import qualified Data.List as List

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options
import qualified Types.Estimate as Estimate
import qualified Types.VerboseOutput as VerboseOutput

import qualified PointEstimate
import qualified Normalization
import qualified Simulation


main :: IO ()
main = do
    opt <- execParser optionDescription
    let norm = Normalization.normalize opt

    yaml $ estimate norm
    Monad.when (Options.verbose opt) $ yaml (VerboseOutput.Type opt norm)

    where
    yaml :: Yaml.ToJSON a => a -> IO ()
    yaml = (ByteString.putStr . Yaml.encode)

    optionDescription = info (options <**> helper)
      ( fullDesc
     <> header "Scrum-style project estimation"
     <> progDescDoc ( paragraphs
            [[ "Estimate how many time intervals are needed for an amount of"
             , "work.  The estimate is expressed as a target number of time"
             , "intervals and a success rate.  It's based on the amount of"
             , "work to do and examples of how much work was done in past"
             , "time intervals."
             ]
            ,[ "For planning purposes, you can also input the amount of work"
             , "you intend to do per time interval and the fraction of that"
             , "work you intend to spend on the project being estimated."
             ]])
     <> footer "https://github.com/bjaress/estimate")


estimate :: NormalizedOptions.Type -> Estimate.Type
estimate norm = Estimate.Type{..}
    where
    target = PointEstimate.sprints norm
    success = (++ "%") $ show $ floor $ 100 * (Simulation.simulate target norm)



options :: Parser Options.Type
options = Options.Type
    <$> argument auto
        ( metavar "DEFINED_WORK"
       <> help "Amount of well-defined work as an integer." )

    <*> argument auto
        ( metavar "PAST_VELOCITIES"
       <> (help . paragraph)
            [ "Examples of past velocities as a bracketed, comma-separated"
            , "list of integers.  For example, \"[30, 50, 10]\" is an input"
            , "of three past velocities.  Each number represents the total"
            , "work done in a time interval across all projects."
            ])

    <*> (optional . option auto)
        ( long "targetVelocity"
       <> metavar "INTEGER"
       <> (help . paragraph)
            [ "Typical or target amount of work done per time interval"
            , "across all projects.  By default, inferred from past"
            , "velocities using a rule of thumb."
            ])

    <*> option auto
        ( long "focus"
       <> (help . paragraph)
            [ "Fraction of velocity (and past velocities) to apply"
            , "to the specific work being estimated.  Default is based on"
            , "a rule of thumb for teams \"dedicated to the top priority.\""
            ]
       <> showDefault
       <> value (2 % 3)
       <> metavar "PERCENT_SEPARATED_FRACTION" )

    <*> option auto
        ( long "undefinedWork"
       <> metavar "INTEGER"
       <> (help . paragraph)
            [ "Amount of work that is expected but not fully defined."
            , "For example, your process might include a final"
            , "best-practices review that seems to request a consistent"
            , "amount of additional work, no matter what practices were"
            , "followed."
            ]
       <> showDefault
       <> value 0 )

    <*> option auto
        ( long "simulationTrials"
       <> metavar "INTEGER"
       <> help "Number of times to simulate completing the project."
       <> showDefault
       <> value 10000 )

    <*> (optional . option auto)
        ( long "simulationSeed"
       <> metavar "INTEGER"
       <> (help . paragraph)
            [ "Seed for random number generator used in simulations"
            , "Default is to derive a seed from other options."
            ])

    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Repeat input and intermediate values in output." )


paragraph :: [String] -> String
paragraph = List.intercalate " "

-- For some reason, you've really got to go around the barn for this.
-- At least, I couldn't find an easier way.
paragraphs :: [[String]] -> Maybe Pretty.Doc
paragraphs =
    Chunk.unChunk . Chunk.vsepChunks . (Chunk.paragraph . List.intercalate " " <$>)
