module Util.Hex where

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Prelude (($))

stripHexPrefix :: String -> String
stripHexPrefix s = fromMaybe s $ stripPrefix (Pattern "0x") s
