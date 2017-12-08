module Data.Ethereum.Abi.Property.Hex where

import Prelude

import Data.Char.Unicode (isHexDigit)
import Data.Foldable (all)
import Data.Int (even)
import Data.String as Str
import Data.String.Utils (startsWith)
import Property ((<&>))
import Test.QuickCheck (Result, (<?>))
import Util.Hex (stripHexPrefix)

hasHexPrefix :: String -> Result
hasHexPrefix s = startsWith "0x" s
  <?> ("Has no '0x' prefix: " <> s)

hasOnlyHexDigits :: String -> Result
hasOnlyHexDigits s = (Str.toCharArray >>> all isHexDigit) s
  <?> ("Has not only hexadecimal digits: " <> s)

nonEmpty :: String -> Result
nonEmpty s = (not Str.null) s <?> "Is empty"

hasEvenLength :: String -> Result
hasEvenLength s = even (Str.length s) <?> "Has odd length"

isHexEncoding :: String -> Result
isHexEncoding = hasHexPrefix
            <&> hasEvenLength
            <&> stripHexPrefix >>> hasOnlyHexDigits

isHex :: String -> Result
isHex = hasHexPrefix
            <&> stripHexPrefix >>> nonEmpty
            <&> stripHexPrefix >>> hasOnlyHexDigits
