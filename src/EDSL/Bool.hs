{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module EDSL.Bool (
  Bool(..),
  pattern False_,
  pattern True_,
) where

import           EDSL.Elt
import           EDSL.TH.Pattern
import           EDSL.Tuple

instance Elt Bool
instance IsTuple Bool

mkPattern ''Bool

