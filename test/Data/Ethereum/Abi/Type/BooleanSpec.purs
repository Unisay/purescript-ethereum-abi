module Data.Ethereum.Abi.Type.Boolean.Spec where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Test.QuickCheck (Result)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "Boolean" do
    test "produces a correct hex encoding" $
      quickCheck $ propTypeEncMultiple32b :: Boolean -> Result
    test "encoded length is multiple 32" $
      quickCheck $ propTypeEncMultiple32b :: Boolean -> Result
    test "encoding roundtrip" $
      quickCheck $ propTypeEncIsDecodable :: Boolean -> Result
