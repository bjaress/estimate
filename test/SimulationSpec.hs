{-# LANGUAGE RecordWildCards #-}

module SimulationSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)


import qualified Types.NormalizedOptions as NormalizedOptions
import Simulation
import qualified TypeGenerators as Generate

import qualified System.Random as Random
import qualified Control.Monad as Control
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))


-- Full simulations are complex and need extra exploration to find
-- corner cases.  For some properties, use this to increase the number
-- of random inputs tried.
hammer = modifyMaxSuccess (* 100)

spec :: Spec
spec = describe "Simulation" $ do

    describe "Full Simulation" $ do

        it "always succeeds when failure is impossible" $ do
            simulate 6 NormalizedOptions.Type
                { work = 30
                , velocity = 10
                , focus = 1 % 2
                , pastVelocities = [10]
                , simulationTrials = 1
                , simulationSeed = 0
                } `shouldBe` 1 % 1

        it "always fails when success is impossible" $ do
            simulate 5 NormalizedOptions.Type
                { work = 30
                , velocity = 10
                , focus = 1 % 2
                , pastVelocities = [10]
                , simulationTrials = 1
                , simulationSeed = 0
                } `shouldBe` 0 % 1

        hammer $ it "more effort gives more success" $ property $ \options ->
            forAll Generate.positive $ \target ->
            forAll Generate.fraction $ \f1 ->
            forAll Generate.fraction $ \f2 ->
                let focused = max f1 f2 in
                let unfocused = min f1 f2 in
                simulate target options {NormalizedOptions.focus = focused} >=
                    simulate target options {NormalizedOptions.focus = unfocused}

        hammer $ it "smaller project gives more success" $ property $ \options ->
            forAll Generate.positive $ \target ->
            forAll Generate.positive $ \w1 ->
            forAll Generate.positive $ \w2 ->
                let moreWork = max w1 w2 in
                let lessWork = min w1 w2 in
                simulate target options {NormalizedOptions.work = lessWork} >=
                    simulate target options {NormalizedOptions.work = moreWork}

        hammer $ it "more time gives more success" $ property $ \options ->
            forAll Generate.positive $ \t1 ->
            forAll Generate.positive $ \t2 ->
                let moreTime = max t1 t2 in
                let lessTime = min t1 t2 in
                simulate moreTime options >= simulate lessTime options

        hammer $ it "higher historical velocity gives more success" $ property $ \options ->
            forAll Generate.positive $ \target ->
            forAll Generate.positive $ \boost ->
                let highVelocity = (+boost) <$> NormalizedOptions.pastVelocities options in
                simulate target options {NormalizedOptions.pastVelocities = highVelocity}
                    >= simulate target options

        hammer $ it "always gives an answer in range" $
            property $ \options ->
            forAll Generate.positive $ \target ->
            let result = simulate target options in
            result >= 0%1 && result <= 1%1

    describe "Success Rate" $ do

        it "counts successes [True, False, True]" $ do
            successRate id [True, False, True] `shouldBe` 2 % 3

        it "counts successes [False, False, True]" $ do
            successRate id [False, False, True] `shouldBe` 1 % 3

        it "counts arbitrary successes" $ property $ \candidates ->
            let rate = successRate id candidates in
            rate >= 0 && rate <= 1

    describe "Bootstrapping" $ do

        it "generates an infinite list" $ do
            let gen = Random.mkStdGen 1
            (length . take 100 . bootstrapSprints gen) [True] `shouldBe` 100

        it "output depends on random generator" $ do
            let genA = Random.mkStdGen 1
            let genB = Random.mkStdGen 2
            (bootstrapSprints genA [True, False]) /=
                (bootstrapSprints genB [True, False])

        it "uses only the items given" $ property $ \simulationSeed items ->
            let gen = Random.mkStdGen simulationSeed in
            let result = bootstrapSprints gen (items::[Int]) in
            let valid = (flip elem) items in
            all valid (take 100 result)


    describe "Sprint List to Sprint Count" $ do

        it "takes no sprints to get to 0" $ do
            countSprints 0 [1, 1 ..] == 0

        it "takes N sprints of 1 for project size N" $ forAll Generate.positive $ \n ->
            countSprints n [1, 1 ..] == n

        it "rounds the number of sprints up" $ do
            countSprints 5 [1, 3, 2, 1] == 3

        it "depends on the order of sprints" $ do
            countSprints 3 [1, 3, 1, 1] == 2 &&
                countSprints 3 [3, 1, 1, 1] == 1

        it "sprints are sufficient (if available)" $
            forAll Generate.positive $ \project ->
            forAll Generate.positiveList $ \finite ->
            let sprints = cycle finite in
            let result = countSprints project sprints in
            sum (take result sprints) >= project

        it "sprints are necessary" $
            forAll Generate.positive $ \project ->
            forAll Generate.positiveList $ \sprints ->
            let result = countSprints project sprints in
            let fewer = max 0 $ result - 1 in
            sum (take fewer sprints) < project

        it "takes more sprints to do more work" $
            forAll Generate.positive $ \projectA ->
            forAll Generate.positive $ \projectB ->
            forAll Generate.positiveList $ \sprints ->
            let (big, small) = (max projectA projectB, min projectA projectB) in
            countSprints big sprints >= countSprints small sprints

        it "takes less time when accomplishing work faster" $
            property $ \options ->
            forAll Generate.positive $ \work ->

            let project = work % 1 in

            let faster = (% 1) <$> NormalizedOptions.pastVelocities options in
            let slower = (NormalizedOptions.focus options *) <$> faster in

            countSprints project faster <= countSprints project slower


instance Arbitrary NormalizedOptions.Type
    where
    arbitrary = Generate.normalizedOptions
    shrink = genericShrink
