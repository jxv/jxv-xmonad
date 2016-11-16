module Test.JxvXMonadSpec (spec) where

import Prelude hiding (log)

import Test.Hspec
import Control.Monad.Trans (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH

import JxvXMonad

spec :: Spec
spec = do
  describe "main" $ do
    it "should" $ do
      () `shouldBe` ()
