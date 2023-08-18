{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module EDSL.Bool (
  Bool(..),
  pattern False_,
  pattern True_,
) where

import EDSL.Elt
import EDSL.Tuple
import EDSL.TH.Pattern

instance Elt Bool
instance IsTuple Bool

mkPattern ''Bool

