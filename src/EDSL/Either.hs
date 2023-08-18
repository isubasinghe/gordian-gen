
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

module EDSL.Either (
  Either(..), 
  pattern Left_, 
  pattern Right_
) where

import EDSL.Elt
import EDSL.Exp
import EDSL.TH.Pattern
import EDSL.TH.IsTuple
import EDSL.Match
import EDSL.Tuple
import EDSL.Bool
import Data.Map

instance (Elt a, Elt b) => Elt (Either a b) where
instance (Elt a, Elt b) => IsTuple (Either a b) where

mkPattern ''Either

liftEither :: (Elt a, Elt b) => Either a b -> Exp (Either a b)
liftEither (Left l) = Left_ (Const (fromElt l))
liftEither (Right r) = Right_ (Const (fromElt r))



