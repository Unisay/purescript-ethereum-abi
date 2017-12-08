module Data.Ethereum.Abi.Type.UnsignedInt.Spec where

import Prelude

import Arbitrary (ArbUnsignedInt40)
import Control.Monad.Eff.Random (RANDOM)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Property.Hex (isHexEncoding)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Data.Newtype (unwrap)
import Test.QuickCheck (Result)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "UnsignedInt" do
    test "produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbUnsignedInt40 -> Result
    test "encoded length is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbUnsignedInt40 -> Result
    test "encoding roundtrip" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbUnsignedInt40 -> Result
