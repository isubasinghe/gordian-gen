{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns        #-}

module SMT where
import Data.Typeable
import Data.BitVector.Sized
import GHC.TypeLits

import EDSL.Exp
import EDSL.Elt
import EDSL.Maybe
import EDSL.Match
import EDSL.Debug
import EDSL.Trace

newtype SUInt a = SUInt { uint :: BV a }
newtype SInt a = SInt { int :: BV a }

data Expr t where
  VAR :: String -> Expr a
  CONST :: a -> Expr a
  ADD :: Expr (SInt k) -> Expr (SInt k) -> Expr (SInt k)
  Func :: Expr [a] -> Expr a -> Expr ([a] -> a)


instance Show (Expr Int) where
  show _ = "Expr"




dummy :: forall a. Elt a => Exp a
dummy = Undef (eltR @a)


preCond :: Exp (Maybe Int) -> Exp Int
preCond = match \case
  Just_ _ -> Const 1
  Nothing_ -> Const 0 





