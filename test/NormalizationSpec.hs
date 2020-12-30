{-# LANGUAGE RecordWildCards #-}

module NormalizationSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Control.Monad as Control

import qualified Types.NormalizedOptions as NormalizedOptions
import qualified Types.Options as Options

import Normalization
import qualified TypeGenerators as Generate
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))

spec :: Spec
spec = describe "Normalization" $ do

    describe "Test Cases" $ Control.mapM_ check testCases

    it "always valid after normalization" $ property (valid . normalize)

    it "validation matches generation" $ property valid

    it "supplies different simulationSeeds" $ do
        let with o w h = o {
              Options.simulationSeed = Nothing
            , Options.definedWork = w
            , Options.pastVelocities = h
            }
        let s = NormalizedOptions.simulationSeed
        let first = with baseOptions 100 [1]
        let second = with baseOptions 200 [1]
        let third = with baseOptions 100 [2]
        s (normalize first) /= s (normalize second) &&
            s (normalize first) /= s (normalize third)

    it "does not base simulationSeed on verbosity" $ property $ \options ->
        let opt = options { Options.simulationSeed = Nothing } in
        let verbose = normalize opt { Options.verbose = True } in
        let terse = normalize opt { Options.verbose = False } in
        NormalizedOptions.simulationSeed verbose ==
            NormalizedOptions.simulationSeed terse


    where

    valid :: NormalizedOptions.Type -> Bool
    valid NormalizedOptions.Type{..} =
        work > 0
        && velocity > 0
        && focus > 0
        && length pastVelocities > 0
        && all (> 0) pastVelocities
        && simulationTrials > 0

    check testCase = it (description testCase) $ do
        normalize (input testCase) `shouldBe` (expected testCase)



data TestCase = TestCase
    { description :: String
    , input :: Options.Type
    , expected :: NormalizedOptions.Type
    }

baseOptions = Options.Type
    { definedWork = 18
    , pastVelocities = [10]
    , targetVelocity = Just 10
    , focus = 9 % 10
    , undefinedWork = 0
    , simulationTrials = 100
    , simulationSeed = Just 0
    , verbose = False
    }

baseExpected = NormalizedOptions.Type
    { work = 18
    , velocity = 10
    , focus = 9 % 10
    , pastVelocities = [10]
    , simulationTrials = 100
    , simulationSeed = 0
    }

testCases :: [TestCase]
testCases =
    [ TestCase
        { description = "Simple, two-sprint project"
        , input = baseOptions
        , expected = baseExpected
        }

    , TestCase
        { description = "Slightly over two sprints"
        , input = baseOptions { Options.definedWork = 19 }
        , expected = baseExpected { NormalizedOptions.work = 19 }
        }

    , TestCase
        { description = "Include Undefined Work"
        , input = baseOptions { Options.undefinedWork = 2 }
        , expected = baseExpected { NormalizedOptions.work = 20 }
        }

    , TestCase
        { description = "Secondary Focus"
        , input = baseOptions { Options.focus = 1 % 10 }
        , expected = baseExpected { NormalizedOptions.focus = 1 % 10 }
        }

    , TestCase
        { description = "historical pastVelocities provided"
        , input = baseOptions
            { Options.pastVelocities = [10, -1, 0, 7] }
        , expected = baseExpected
            { NormalizedOptions.pastVelocities = [10, 1, 1, 7] }
        }

    , TestCase
        { description = "typical velocity inferred from pastVelocities"
        , input = baseOptions
            { Options.pastVelocities = [3, 1, 2]
            , Options.targetVelocity = Nothing }
        , expected = baseExpected
            { NormalizedOptions.pastVelocities = [3, 1, 2]
            , NormalizedOptions.velocity = 2 }
        }

    , TestCase
        { description = "another velocity inferred from pastVelocities"
        , input = baseOptions
            { Options.pastVelocities = [40, 30, 50, 60, 70, 10, 100]
            , Options.targetVelocity = Nothing }
        , expected = baseExpected
            { NormalizedOptions.pastVelocities = [40, 30, 50, 60, 70, 10, 100]
            , NormalizedOptions.velocity = 30 }
        }

    , TestCase
        { description = "Realistic Scenario A"
        , input = Options.Type
            { definedWork = 65
            , pastVelocities = [45]
            , targetVelocity = Nothing
            , focus = 2 % 3
            , undefinedWork = 0
            , simulationTrials = 10000
            , simulationSeed = Just 1
            , verbose = False
            }
        , expected = NormalizedOptions.Type
            { work = 65
            , velocity = 45
            , focus = 2 % 3
            , pastVelocities = [45]
            , simulationTrials = 10000
            , simulationSeed = 1
            }
        }

    , TestCase
        { description = "Realistic Scenario B"
        , input = Options.Type
            { definedWork = 23
            , pastVelocities = [45, 22, 56]
            , targetVelocity = Just 30
            , focus = 2 % 3
            , undefinedWork = 30
            , simulationTrials = 10000
            , simulationSeed = Just 100
            , verbose = False
            }
        , expected = NormalizedOptions.Type
            { work = 53
            , velocity = 30
            , focus = 2 % 3
            , pastVelocities = [45, 22, 56]
            , simulationTrials = 10000
            , simulationSeed = 100
            }
        }

    , TestCase
        { description = "Realistic Scenario C"
        , input = Options.Type
            { definedWork = 100
            , pastVelocities = [20, 40, 50]
            , targetVelocity = Just 43
            , focus = 2 % 3
            , undefinedWork = 30
            , simulationTrials = 10000
            , simulationSeed = Just (-73)
            , verbose = False
            }
        , expected = NormalizedOptions.Type
            { work = 130
            , velocity = 43
            , focus = 2 % 3
            , pastVelocities = [20, 40, 50]
            , simulationTrials = 10000
            , simulationSeed = -73
            }
        }
    ]

instance Arbitrary NormalizedOptions.Type
    where
    arbitrary = Generate.normalizedOptions
    shrink = genericShrink

instance Arbitrary Options.Type
    where
    arbitrary = Generate.options
    shrink = genericShrink
