module EDSL.Debug (
  module EDSL.Debug,
  module Text.Printf,
) where

import qualified Debug.Trace as T
import           Text.Printf

dEBUG :: Bool
dEBUG = False

trace :: String -> a -> a
trace msg x =
  if dEBUG
     then T.trace msg x
     else x
