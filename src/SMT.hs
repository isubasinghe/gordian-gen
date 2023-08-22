{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SMT where

import Data.BitVector.Sized
import Data.SCargot
import Data.SCargot.Repr.Basic
import qualified Data.Text as T
import Data.Typeable
import EDSL.Bool
import EDSL.Debug
import EDSL.Elt
import EDSL.Exp
import EDSL.Match
import EDSL.Maybe
import EDSL.Trace
import GHC.Generics
import GHC.TypeLits

data Expr t where
  VAR :: String -> Expr a
  CONST :: a -> Expr a
  ADD :: Expr (SInt k) -> Expr (SInt k) -> Expr (SInt k)
  FUNC :: Expr [b] -> Expr a -> Expr ([b] -> a)
  APPLY :: Expr [a] -> Expr ([a] -> b) -> Expr b
  ITE :: Expr a -> (Expr a -> Expr b) -> (Expr a -> Expr b) -> Expr b
  UIasBV :: Expr (SUInt k) -> Expr (BV k)
  IasBV :: Expr (SInt k) -> Expr (BV k)
  EXTRACTBV :: ix + w' <= w => NatRepr ix -> NatRepr w' -> Expr (BV w) -> Expr (BV w')
  CONCATBV :: (ix + w' <= w, w <= ix + w') => NatRepr ix -> NatRepr w' -> Expr (BV ix) -> Expr (BV w') -> Expr (BV w)

data Atom
  = AAdd
  | ASub
  deriving (Show)

{- fromExpr :: Expr a -> SExpr Atom
fromExpr (VAR s) = undefined
fromExpr (CONST c) = undefined
fromExpr (ADD a b) = undefined
fromExpr (FUNC _ _) = undefined
fromExpr (APPLY _ _) = undefined -}

liftExpr :: Expr t -> Exp (Expr t)
liftExpr (VAR _) = undefined
liftExpr _ = undefined

dummy :: forall a. Elt a => Exp a
dummy = Undef (eltR @a)

true_ :: Exp Bool
true_ = True_

preCond :: Exp (Maybe Int) -> Exp Bool
preCond = match \case
  Just_ _ -> True_
  Nothing_ -> true_

mkFunction1 :: (Elt a, Elt b) => Idx a -> (Exp a -> Exp b) -> Exp (FuncIdx b)
mkFunction1 n fn =
  let var = Var n
   in Func n (fn var)

preCond' :: Exp (FuncIdx Bool)
preCond' = mkFunction1 (Idx "mvalue") preCond

toSMTExpr :: (Elt a) => Exp a -> Expr b
toSMTExpr = \case
  Const c -> undefined
  Var ix -> VAR "ix"
  Let (Idx v) a b -> undefined
  Lam (Idx v) b -> undefined
  App f x -> undefined
  Tuple t -> undefined
  Prj tix t -> undefined
  Unroll e -> undefined
  Roll e -> undefined
  Undef {} -> error "undef"
  Match _ e -> error "match"
  Case x xs -> undefined
  Eq x y -> undefined
  Func v a -> undefined
  _ -> error "undefined"

dumpSMT :: Exp (Expr b) -> Expr b
dumpSMT = undefined

data Foo :: Nat -> * where
  SmallFoo :: (n <= 2) => Foo n
  BigFoo :: (3 <= n) => Foo n

myFoo :: Foo 4
myFoo = BigFoo
