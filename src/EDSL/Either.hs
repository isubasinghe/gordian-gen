
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

module EDSL.Either (
  Either(..),
  pattern Left_,
  pattern Right_
) where

import           Data.Map
import           EDSL.Bool
import           EDSL.Elt
import           EDSL.Exp
import           EDSL.Match
import           EDSL.TH.IsTuple
import           EDSL.TH.Pattern
import           EDSL.Tuple

instance (Elt a, Elt b) => Elt (Either a b) where
instance (Elt a, Elt b) => IsTuple (Either a b) where

mkPattern ''Either

liftEither :: (Elt a, Elt b) => Either a b -> Exp (Either a b)
liftEither (Left l)  = Left_ (Const (fromElt l))
liftEither (Right r) = Right_ (Const (fromElt r))



