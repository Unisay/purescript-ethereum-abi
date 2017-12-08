module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude

import Arbitrary (ArbSignedInt40)
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
  suite "SignedInt" do
    test "produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbSignedInt40 -> Result
    test "encoded length is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt40 -> Result
    test "encoding roundtrip" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbSignedInt40 -> Result
