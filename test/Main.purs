module Test.Main where

import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Data.Ethereum.Abi.Type.UnsignedInt.Spec as UnsignedInt
import Data.Ethereum.Abi.Type.SignedInt.Spec as SignedInt
import Data.Ethereum.Abi.Type.Boolean.Spec as Boolean
import Data.Ethereum.Abi.Type.Bytes.Spec as Bytes

main :: ∀ e. Eff ( console    :: CONSOLE
                 , testOutput :: TESTOUTPUT
                 , avar       :: AVAR
                 , random     :: RANDOM
                 | e
                 ) Unit
main = runTest do
  UnsignedInt.spec
  SignedInt.spec
  Boolean.spec
  Bytes.spec
