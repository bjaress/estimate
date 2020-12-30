{-# LANGUAGE RecordWildCards #-}

module LibSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Control.Monad as Control

import qualified Data.Ratio as Ratio
import Data.Ratio ((%))

import qualified TypeGenerators as Generate
import qualified Hammer as Hammer

import qualified Types.EstimationResult as EstimationResult
import qualified Types.Estimate as Estimate
import qualified Types.Options as Options

import Lib

spec :: Spec
spec = describe "Library" $ do

    describe "Test Cases" $ Control.mapM_ check testCases

    Hammer.it "Trying to go faster is riskier" $ property $ \seed v1 v2 options ->
        let slow = min v1 v2 in
        let fast = max v1 v2 in
        let base = options { Options.simulationSeed = Just seed } in
        let safer = base { Options.targetVelocity = Just slow } in
        let riskier = base { Options.targetVelocity = Just fast } in

        let safeResult = estimate safer in
        let riskyResult = estimate riskier in

            success riskyResult <= success safeResult &&
                target riskyResult <= target safeResult


target :: EstimationResult.Type -> Int
target = Estimate.target . EstimationResult.estimate
success :: EstimationResult.Type -> Int
success = read . takeWhile (/= '%') . Estimate.success . EstimationResult.estimate


data TestCase = TestCase
    { description :: String
    , input :: Options.Type
    , expected :: Estimate.Type
    }

check TestCase{..} = it description $
    (EstimationResult.estimate . estimate) input `shouldBe` expected

testCases :: [TestCase]
testCases =
    [ TestCase
        { description = "First example from README"
        , input = Options.Type
            { definedWork = 100
            , pastVelocities = [65, 30, 50, 70, 40, 90]
            , targetVelocity = Nothing
            , focus = 2 % 3
            , undefinedWork = 0
            , simulationTrials = 10000
            , simulationSeed = Nothing
            , verbose = False
            }
        , expected = Estimate.Type
              { target = 4
              , success = "97%"
              }
        }
    , TestCase
        { description = "Second example from README"
        , input = Options.Type
            { definedWork = 100
            , pastVelocities = [65, 30, 50, 70, 40, 90]
            , targetVelocity = Just 50
            , focus = 2 % 3
            , undefinedWork = 0
            , simulationTrials = 10000
            , simulationSeed = Nothing
            , verbose = False
            }
        , expected = Estimate.Type
              { target = 3
              , success = "70%"
              }
        }
    , TestCase
        { description = "Third example from README"
        , input = Options.Type
            { definedWork = 100
            , pastVelocities = [65, 30, 50, 70, 40, 90]
            , targetVelocity = Nothing
            , focus = 1 % 2
            , undefinedWork = 0
            , simulationTrials = 10000
            , simulationSeed = Nothing
            , verbose = False
            }
        , expected = Estimate.Type
              { target = 5
              , success = "98%"
              }
        }
    ]


instance Arbitrary Options.Type
    where
    arbitrary = Generate.options
    shrink = genericShrink
