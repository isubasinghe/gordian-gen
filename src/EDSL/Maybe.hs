{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module EDSL.Maybe (
  Maybe(..), liftMaybe,
  pattern Nothing_,
  pattern Just_
) where

import           Data.Map
import           EDSL.Elt
import           EDSL.Exp
import           EDSL.Match
import           EDSL.TH.IsTuple
import           EDSL.TH.Pattern
import           EDSL.Tuple

instance Elt a => Elt (Maybe a) where
instance Elt a => IsTuple (Maybe a) where

mkPattern ''Maybe

liftMaybe :: Elt a => Maybe a -> Exp (Maybe a)
liftMaybe Nothing  = Nothing_
liftMaybe (Just x) = Just_ (Const (fromElt x))

