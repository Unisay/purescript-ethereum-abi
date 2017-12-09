module Util.Hex where

import Prelude

import Data.Array as Arr
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), fromCharArray, length, stripPrefix)

stripHexPrefix :: String -> String
stripHexPrefix s = fromMaybe s $ stripPrefix (Pattern "0x") s

pad64 :: Char -> String -> String
pad64 _ "0" = "0000000000000000000000000000000000000000000000000000000000000000"
pad64 padChar s = fromCharArray (Arr.replicate delta padChar) <> s
  where
    delta = targetLen - actualLen
    targetLen = let d = actualLen `div` 64
                    m = actualLen `mod` 64
                    a = min 1 m
                in (d + a) * 64
    actualLen = length s
