module Data.Ethereum.Abi.Class
  ( class AbiType
  , enc
  , dec
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Binary as Bin
import Data.Binary.BaseN (Radix(..), fromStringAs, toStringAs)
import Data.Binary.SignedInt (SignedInt, fromUnsignedUnsafe, isNegative, toString2c)
import Data.Binary.UnsignedInt (UnsignedInt)
import Data.Either (Either, note)
import Data.Ethereum.Abi.Type.Class (class Dividend8)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (D32)
import Util.Hex (pad64, stripHexPrefix)

class AbiType a where
  enc :: a -> String
  dec :: String -> Either String a

{-
  uint<M>: enc(X) is the big-endian encoding of X,
  padded on the higher-order (left) side with zero-bytes
  such that the length is a multiple of 32 bytes.
-}
instance abiTypeUnsignedInt :: Dividend8 m => AbiType (UnsignedInt m) where
  enc ui = "0x" <> pad64 '0' (toStringAs Hex ui)
  dec s = note "Failed to decode string as UnsignedInt" $ fromStringAs Hex (stripHexPrefix s)

{-
  int<M>: enc(X) is the big-endian two’s complement encoding of X,
  padded on the higher-oder (left) side with 0xff for negative X
  and with zero bytes for positive X
  such that the length is a multiple of 32 bytes.
-}
instance abiTypeSignedInt :: (Dividend8 m) => AbiType (SignedInt m) where
  enc si = "0x" <> pad64 (if isNegative si then 'f' else '0') (toString2c Hex si)
  dec s = note "Failed to decode string as SignedInt" $ fromUnsignedUnsafe <$> u
    where u :: Maybe (UnsignedInt m)
          u = fromStringAs Hex (stripHexPrefix s)


{- bool: as in the uint8 case, where 1 is used for true and 0 for false -}
instance abiTypeBoolean :: AbiType Boolean where
  enc true  = enc (one  :: UnsignedInt D32)
  enc false = enc (zero :: UnsignedInt D32)
  dec = decUint >=> toBool where
    toBool  :: UnsignedInt D32 -> Either String Boolean
    toBool ui | ui == one = pure true
    toBool ui | Bin.isZero ui = pure false
    toBool _ = throwError "Failed to decode string as Boolean"
    decUint :: String -> Either String (UnsignedInt D32)
    decUint = dec
