module Data.Ethereum.Abi.Type.Bytes
  ( Bytes
  , mkBytes
  , takeBytes
  ) where

import Prelude

import Data.Binary (Bits)
import Data.Binary as Bin
import Data.Either (note)
import Data.Ethereum.Abi.Class (class AbiType)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Typelevel.Num (class Pos)
import Data.Typelevel.Num as Nat
import Data.Typelevel.Undefined (undefined)
import Util.Hex (pad64, stripHexPrefix)

newtype Bytes m = Bytes Bits

mkBytes :: ∀ m . Pos m => Bits -> Maybe (Bytes m)
mkBytes bits | Bin.length bits <= 8 * Nat.toInt (undefined :: m) = Just (Bytes bits)
mkBytes _ = Nothing

-- | discards exceeding bits
takeBytes :: ∀ m . Pos m => Bits -> Bytes m
takeBytes bits = Bytes $ Bin.take numBits bits
  where
    numBits = numBytes * 8
    numBytes = Nat.toInt (undefined :: m)

instance showBytes :: Show (Bytes m) where
  show (Bytes bits) = "Bytes#" <> Bin.toHexString bits

derive newtype instance eqBytes :: Eq (Bytes m)

instance abiTypeBytes :: Pos m => AbiType (Bytes m) where
  enc (Bytes bits) = "0x" <> pad64 '0' (Bin.toHexString bits)
  dec s = note "Failed to decode string as Bits" $ Bytes <$> Bin.fromHexString (stripHexPrefix s)
