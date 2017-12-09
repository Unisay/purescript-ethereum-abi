module Data.Ethereum.Abi.Type.UnsignedInt.Spec where

import Prelude

import Arbitrary (ArbInt(..))
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary.UnsignedInt (UnsignedInt, fromInt)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Property.Hex (isHexEncoding)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Data.Newtype (class Newtype, unwrap)
import Data.Typelevel.Num (D40, d40)
import Test.QuickCheck (class Arbitrary, Result, arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

newtype ArbUnsignedInt40 = ArbUnsignedInt40 (UnsignedInt D40)
derive newtype instance showArbUnsignedInt40 :: Show ArbUnsignedInt40
derive instance newtypeUnsignedInt40 :: Newtype ArbUnsignedInt40 _
instance arbitraryUnsignedInt40 :: Arbitrary ArbUnsignedInt40 where
  arbitrary = do
    (ArbInt int) <- arbitrary
    pure $ ArbUnsignedInt40 $ fromInt d40 int


spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "UnsignedInt" do
    test "produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbUnsignedInt40 -> Result
    test "encoded length is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbUnsignedInt40 -> Result
    test "encoding roundtrip" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbUnsignedInt40 -> Result
