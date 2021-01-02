{-# LANGUAGE RecordWildCards #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Options.Applicative.Help.Pretty as Pretty
import qualified Options.Applicative.Help.Chunk as Chunk
import Options.Applicative.Help.Chunk ((<</>>))
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.UTF8 as UTF8
import GHC.Generics (Generic)

import qualified Data.Yaml as Yaml
import Data.Ratio ((%))
import qualified Data.ByteString as ByteString
import qualified Data.List as List


import qualified Types.Options as Options
import qualified Types.EstimationResult as EstimationResult
import qualified Types.ProgramInfo as ProgramInfo

import qualified Lib

programInfo :: ProgramInfo.Type
programInfo = ProgramInfo.Type{..}
    where

    version = "1.3.0"

    namespace = "bjaress"
    name = "estimate"
    notice = "Copyright 2020 Brian Jaress"

    url = "https://github.com/" ++ namespace ++ "/" ++ name

main :: IO ()
main = do
    opt <- execParser optionDescription
    let result = Lib.estimate opt
    prettyPrint opt result

    where

    optionDescription = info (options <**> helper <**> infoOpt)
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
             ]
            ,[ "Whether you believe the estimates is up to you."
             ]])
       <> footer (ProgramInfo.url programInfo))

    infoOpt = infoOption (UTF8.toString $ Yaml.encode programInfo)
        ( long "info"
       <> help "Print program info and exit.")


prettyPrint :: Options.Type -> EstimationResult.Type -> IO ()
prettyPrint opt result
    | Options.verbose opt = yaml result
    | otherwise = yaml $ EstimationResult.estimate result
    where
    yaml :: Yaml.ToJSON a => a -> IO ()
    yaml = (ByteString.putStr . Yaml.encode)


paragraph :: [String] -> String
paragraph = List.intercalate " "

-- Lists of lines that are run together (re-flowed) within each sublist,
-- then separated by a blank line
paragraphs :: [[String]] -> Maybe Pretty.Doc
paragraphs =
    Chunk.unChunk . Chunk.vsepChunks . (Chunk.paragraph . paragraph <$>)

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
        ( long "target-velocity"
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
        ( long "undefined-work"
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
        ( long "simulation-trials"
       <> metavar "INTEGER"
       <> help "Number of times to simulate completing the project."
       <> showDefault
       <> value 10000 )

    <*> (optional . option auto)
        ( long "simulation-seed"
       <> metavar "INTEGER"
       <> (help . paragraph)
            [ "Seed for random number generator used in simulations"
            , "Default is to derive a seed from other options."
            ])

    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Include intermediate values in output." )

