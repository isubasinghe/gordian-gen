{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns        #-}

module SMT where
import           Data.BitVector.Sized
import           Data.Typeable
import           GHC.TypeLits

import           EDSL.Bool
import           EDSL.Debug
import           EDSL.Elt
import           EDSL.Exp
import           EDSL.Match
import           EDSL.Maybe
import           EDSL.Trace

data Expr t where
  VAR :: String -> Expr a
  CONST :: a -> Expr a
  ADD :: Expr (SInt k) -> Expr (SInt k) -> Expr (SInt k)
  Func :: Expr [a] -> Expr a -> Expr ([a] -> a)


instance Show (Expr Int) where
  show _ = "Expr"




dummy :: forall a. Elt a => Exp a
dummy = Undef (eltR @a)


preCond :: Exp (Maybe Int) -> Exp Bool
preCond = match \case
  Just_ a  -> a `Eq` (Const 3)
  Nothing_ -> False_
