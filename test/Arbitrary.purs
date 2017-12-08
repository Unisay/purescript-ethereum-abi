module Arbitrary where

import Prelude

import Data.Binary.SignedInt (SignedInt)
import Data.Binary.SignedInt as SI
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Binary.UnsignedInt as UI
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D40, d40)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (frequency)

newtype ArbInt = ArbInt Int
derive instance newtypeArbInt :: Newtype ArbInt _
derive newtype instance eqArbInt :: Eq ArbInt
instance arbitraryInt :: Arbitrary ArbInt where
  arbitrary = ArbInt <$> frequency gens where
    gens = Tuple 0.05 (pure 0)      :|
           Tuple 0.05 (pure 1)      :
           Tuple 0.05 (pure (-1))   :
           Tuple 0.05 (pure top)    :
           Tuple 0.05 (pure bottom) :
           Tuple 0.75 arbitrary     :
           Nil

newtype ArbUnsignedInt40 = ArbUnsignedInt40 (UnsignedInt D40)
derive newtype instance showArbUnsignedInt40 :: Show ArbUnsignedInt40
derive instance newtypeUnsignedInt40 :: Newtype ArbUnsignedInt40 _
instance arbitraryUnsignedInt40 :: Arbitrary ArbUnsignedInt40 where
  arbitrary = do
    (ArbInt int) <- arbitrary
    pure $ ArbUnsignedInt40 $ UI.fromInt d40 int

newtype ArbSignedInt40 = ArbSignedInt40 (SignedInt D40)
derive newtype instance showArbSignedInt40 :: Show ArbSignedInt40
derive instance newtypeSignedInt40 :: Newtype ArbSignedInt40 _
instance arbitrarySignedInt40 :: Arbitrary ArbSignedInt40 where
  arbitrary = do
    (ArbInt int) <- arbitrary
    pure $ ArbSignedInt40 $ SI.fromInt d40 int
