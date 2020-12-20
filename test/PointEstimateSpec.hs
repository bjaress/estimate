{-# LANGUAGE RecordWildCards #-}

module PointEstimateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Control.Monad as Control
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))

import qualified Types.NormalizedOptions as NormalizedOptions

import qualified TypeGenerators as Generate
import PointEstimate


spec :: Spec
spec = describe "Sprints" $ do

    describe "Test Cases" $ Control.mapM_ check testCases

    it "more velocity => fewer sprints" $ property $ \options ->
        let faster = options {
            NormalizedOptions.velocity = 1 + NormalizedOptions.velocity options } in
        sprints faster <= sprints options

    it "more work => more sprints" $ property $ \options ->
        let bigger = options {
            NormalizedOptions.work = 1 + NormalizedOptions.work options } in
        sprints bigger >= sprints options

    it "less focus => more sprints" $ property $ \options ->
        let lessFocus = options {
            NormalizedOptions.focus = (1 % 2) * NormalizedOptions.focus options} in
        sprints lessFocus >= sprints options

    where
    check testCase = it (description testCase) $ do
        sprints (input testCase) `shouldBe` (expected testCase)




instance Arbitrary NormalizedOptions.Type
    where
    arbitrary = Generate.normalizedOptions
    shrink = genericShrink


data TestCase = TestCase
    { description :: String
    , input :: NormalizedOptions.Type
    , expected :: Int
    }

-- todo turn into NormalizedOptions.Type test cases
testCases :: [TestCase]
testCases =
    [ TestCase
        { description = "Simple, two-sprint project"
        , input = NormalizedOptions.Type
            { work = 18
            , velocity = 10
            , focus = 9 % 10
            , pastVelocities = [10]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 2
        }

    , TestCase
        { description = "Slightly over two sprints"
        , input = NormalizedOptions.Type
            { work = 19
            , velocity = 10
            , focus = 9 % 10
            , pastVelocities = [10]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 3
        }

    , TestCase
        { description = "Secondary priority"
        , input = NormalizedOptions.Type
            { work = 18
            , velocity = 10
            , focus = 1 % 10
            , pastVelocities = [10]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 18
        }

    , TestCase
        { description = "Larger project"
        , input = NormalizedOptions.Type
            { work = 48
            , velocity = 10
            , focus = 9 % 10
            , pastVelocities = [10]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 6
        }

    , TestCase
        { description = "Realistic Scenario A"
        , input = NormalizedOptions.Type
            { work = 65
            , velocity = 45
            , focus = 2 % 3
            , pastVelocities = [45]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected =  3
        }

    , TestCase
        { description = "Realistic Scenario B"
        , input = NormalizedOptions.Type
            { work = 53
            , velocity = 45
            , focus = 2 % 3
            , pastVelocities = [45]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 2
        }

    , TestCase
        { description = "Realistic Scenario C"
        , input = NormalizedOptions.Type
            { work = 130
            , velocity = 45
            , focus = 2 % 3
            , pastVelocities = [45]
            , simulationTrials = 1000
            , simulationSeed = 0
            }
        , expected = 5
        }

    ]
