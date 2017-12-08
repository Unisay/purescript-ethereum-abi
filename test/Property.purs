module Property where


import Prelude

import Data.Foldable (all)
import Data.Int (even)
import Data.Newtype (class Newtype, unwrap)
import Data.String (length, null, toCharArray)
import Data.String.Utils (startsWith)
import Test.QuickCheck (Result(..), (<?>))

newtype Results = Results Result

derive instance newtypeResults :: Newtype Results _

instance semigroupResults :: Semigroup Results where
  append (Results (Failed l)) (Results (Failed s)) = Results $ Failed $ l <> "; " <>  s
  append (Results f@(Failed l)) _ = Results $ f
  append _ (Results f@(Failed l)) = Results $ f
  append r _ = r

and :: ∀ a. (a -> Result) -> (a -> Result) -> a -> Result
and f g = (f >>> Results <> g >>> Results) >>> unwrap

infix 2 and as <&>

and' :: Result -> Result -> Result
and' l r = unwrap (Results l <> Results r)

infix 2 and' as <&&>
