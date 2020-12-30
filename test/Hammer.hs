
module Hammer (it) where

import qualified Test.Hspec as Hspec
import qualified Test.Hspec.Core.QuickCheck as QuickCheck

-- Full simulations are complex and need extra exploration to find
-- corner cases.  For some properties, use this to increase the number
-- of random inputs tried.
it :: Hspec.Example a => String -> a -> Hspec.SpecWith (Hspec.Arg a)
it desc arg = QuickCheck.modifyMaxSuccess (* 100) $ Hspec.it desc arg
