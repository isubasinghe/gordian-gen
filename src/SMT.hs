{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SMT where

import Data.BitVector.Sized
import Data.Typeable
import EDSL.Bool
import EDSL.Debug
import EDSL.Elt
import EDSL.Exp
import EDSL.Match
import EDSL.Maybe
import EDSL.Trace
import qualified Data.Text as T
import GHC.TypeLits
import GHC.Generics

data Expr t where
  VAR :: String -> Expr a
  CONST :: a -> Expr a
  ADD :: Expr (SInt k) -> Expr (SInt k) -> Expr (SInt k)
  Func :: Expr [a] -> Expr a -> Expr ([a] -> a)

instance Show (Expr Int) where
  show _ = "Expr"

dummy :: forall a. Elt a => Exp a
dummy = Undef (eltR @a)

true_ :: Exp Bool 
true_ = True_

preCond :: Exp (Maybe Int) -> Exp Bool 
preCond x = 
  let body = match \case 
              Just_ _ -> true_ 
              Nothing_ -> true_
      l = Idx "preCond"
      v = Idx "x"
  in 
    Let l (Lam v (body (Var v))) (App (Var l) x)
    


mkFunc :: (Exp a -> Exp b) -> Exp (Expr (SMTConvertT b))
mkFunc _ = undefined


type family SMTConvertT a where
  SMTConvertT (Maybe Int) = Int
