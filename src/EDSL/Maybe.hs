{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module EDSL.Maybe (
  Maybe(..), liftMaybe,
  pattern Nothing_,
  pattern Just_
) where

import EDSL.Elt
import EDSL.Exp
import EDSL.TH.Pattern
import EDSL.TH.IsTuple
import EDSL.Match
import EDSL.Tuple
import Data.Map

instance Elt a => Elt (Maybe a) where
instance Elt a => IsTuple (Maybe a) where

mkPattern ''Maybe

liftMaybe :: Elt a => Maybe a -> Exp (Maybe a)
liftMaybe Nothing  = Nothing_
liftMaybe (Just x) = Just_ (Const (fromElt x))

