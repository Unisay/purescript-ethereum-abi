module Data.Ethereum.Abi.Type.SignedInt.Spec where

import Prelude

import Arbitrary (ArbInt(ArbInt))
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary.SignedInt (SignedInt, fromInt)
import Data.Ethereum.Abi.Class (enc)
import Data.Ethereum.Abi.Property.Hex (isHexEncoding)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Data.Newtype (class Newtype, unwrap)
import Data.Typelevel.Num (type (:*), D2, D40, D5, D6, d40)
import Data.Typelevel.Undefined (undefined)
import Test.QuickCheck (class Arbitrary, Result, arbitrary)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

newtype ArbSignedInt40 = ArbSignedInt40 (SignedInt D40)
derive newtype instance showArbSignedInt40 :: Show ArbSignedInt40
derive instance newtypeSignedInt40 :: Newtype ArbSignedInt40 _
instance arbitrarySignedInt40 :: Arbitrary ArbSignedInt40 where
  arbitrary = arbitrary <#> \(ArbInt int) ->  ArbSignedInt40 (fromInt d40 int)

newtype ArbSignedInt256 = ArbSignedInt256 (SignedInt (D2 :* D5 :* D6))
derive newtype instance showArbSignedInt256 :: Show ArbSignedInt256
derive instance newtypeSignedInt256 :: Newtype ArbSignedInt256 _
instance arbitrarySignedInt256 :: Arbitrary ArbSignedInt256 where
  arbitrary = arbitrary <#> \(ArbInt int) ->  ArbSignedInt256 (fromInt d256 int) where
    d256 :: D2 :* D5 :* D6
    d256 = undefined


spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "SignedInt" do
    test "produces a correct hex encoding" $
      quickCheck $ (unwrap >>> enc >>> isHexEncoding) :: ArbSignedInt40 -> Result
    test "encoded length is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbSignedInt40 -> Result
    test "encoding roundtrip" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbSignedInt256 -> Result
