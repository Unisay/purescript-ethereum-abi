module Data.Ethereum.Abi.Type.Bytes.Spec where

import Prelude

import Arbitrary (ArbBit)
import Control.Monad.Eff.Random (RANDOM)
import Data.Binary (Bits(Bits))
import Data.Ethereum.Abi.Type (Bytes, takeBytes)
import Data.Ethereum.Abi.Type.Property (propTypeEncIsDecodable, propTypeEncMultiple32b)
import Data.Newtype (class Newtype, unwrap)
import Data.Typelevel.Num (D64, D40)
import Test.QuickCheck (class Arbitrary, Result, arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

newtype ArbBytes40 = ArbBytes40 (Bytes D40)
derive instance newtypeArbBytes40 :: Newtype ArbBytes40 _
instance arbitraryBytes40 :: Arbitrary ArbBytes40 where
  arbitrary = ArbBytes40 <$> takeBytes <$> Bits <$> vectorOf 320 arbBit where
    arbBit = (arbitrary :: Gen ArbBit) <#> unwrap

newtype ArbBytes64 = ArbBytes64 (Bytes D64)
derive instance newtypeArbBytes64 :: Newtype ArbBytes64 _
instance arbitraryBytes64 :: Arbitrary ArbBytes64 where
  arbitrary = ArbBytes64 <$> takeBytes <$> Bits <$> vectorOf 512 arbBit where
    arbBit = (arbitrary :: Gen ArbBit) <#> unwrap

spec :: ∀ e. TestSuite (random :: RANDOM | e)
spec = do
  suite "Bytes" do
    test "produces a correct hex encoding" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbBytes40 -> Result
    test "encoded length is multiple 32" $
      quickCheck $ (unwrap >>> propTypeEncMultiple32b) :: ArbBytes40 -> Result
    test "encoding roundtrip" $
      quickCheck $ (unwrap >>> propTypeEncIsDecodable) :: ArbBytes64 -> Result
