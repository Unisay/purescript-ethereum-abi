module Arbitrary where

import Prelude

import Data.Binary (Bit(..))
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

newtype ArbBit = ArbBit Bit
derive instance newtypeArbBit :: Newtype ArbBit _
derive newtype instance eqArbBit :: Eq ArbBit
derive newtype instance showArbBit :: Show ArbBit
instance arbitraryBit :: Arbitrary ArbBit where
  arbitrary = ArbBit <<< Bit <$> arbitrary

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
