{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module EAST where

import Data.BitVector.Sized
import Data.Data
import Data.Kind ()
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.SCargot
import Data.SCargot.Repr.Basic
import qualified Data.Text as T
import Data.Typeable
import EDSL.Exp
import GHC.Float (int2Float)
import GHC.Generics
  ( C,
    Constructor,
    D,
    Datatype,
    Generic (Rep),
    K1,
    M1,
    U1,
    conName,
    datatypeName,
    type (:*:),
    type (:+:),
  )
import GHC.TypeLits
import SMT

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

x = HCons 23 (HCons "ABC" HNil)

data MaybeConstructors where
  MaybeCJust :: MaybeConstructors
  MaybeCNothing :: MaybeConstructors

{- data NextRecvConstructors where
  NextRecvCNotification :: NextRecvConstructors
  NextRecvCPPCall :: NextRecvConstructors
  NextRecvCUnknown :: NextRecvConstructors -}

{- constructorNextRecvNotification :: Expr TNextRecvC
constructorNextRecvNotification = ETNEXTRECV 0 -}

{- constructorNextRecvPPCall = ETNEXTRECV 1

constructorNextRecvUnknown = ETNEXTRECV 2 -}

constructorMaybeNothing :: Expr TMaybeC
constructorMaybeNothing = ETMAYBE 0

constructorMaybeJust :: Expr TMaybeC
constructorMaybeJust = ETMAYBE 1

class Finite a where
  numElems :: Int

instance Finite 'TCh where
  numElems = 63

data Type where
  TBV32 :: Type
  TBV64 :: Type
  TBool :: Type
  TCh :: Type
  TPD :: Type
  TSet :: Type -> Type
  TMsgInfo :: Type
  TMaybe :: Type -> Type
  TMaybeC :: Type
  TTuple :: Type -> Type -> Type
  TComposite :: Type
  deriving (Typeable)

{- TNextRecv :: Type
TNextRecvC :: Type -}

class TBitRepr a where
  tbvsz :: Int
  tname :: String
  tfields :: [Type]
  default tfields :: [Type]
  tfields = []

instance TBitRepr 'TBV32 where
  tbvsz = 32
  tname = "BV32"

instance TBitRepr 'TBV64 where
  tbvsz = 64
  tname = "BV64"

instance TBitRepr 'TBool where
  tbvsz = 1
  tname = "BVBool"

instance TBitRepr 'TCh where
  tbvsz = 8
  tname = "Ch"

instance TBitRepr 'TPD where
  tbvsz = 8
  tname = "PD"

instance TBitRepr 'TMsgInfo where
  tbvsz = 32
  tname = "MsgInfo"

instance TBitRepr 'TMaybeC where
  tbvsz = 1
  tname = "MaybeC"

instance (TBitRepr a, Finite a) => TBitRepr (TSet a) where
  tbvsz = (numElems @a) * (tbvsz @a)
  tname = "Set_of_" ++ (tname @a)

instance (TBitRepr a, TBitRepr b) => TBitRepr (TTuple a b) where
  tbvsz = (tbvsz @a) + (tbvsz @b)
  tname = (tname @a) ++ "_prod_" ++ (tname @b)

instance (TBitRepr a) => TBitRepr (TMaybe a) where
  tbvsz = (tbvsz @TMaybeC) + (tbvsz @a)
  tname = "Maybe_of_" ++ (tname @a)

data Expr (t :: Type) where
  VAR :: (Typeable a, TBitRepr a) => String -> Expr a
  EBV32 :: Int -> Expr TBV32
  EQU :: (Typeable a, TBitRepr a) => Expr a -> Expr a -> Expr TBool
  EADD :: Expr TBV32 -> Expr TBV32 -> Expr TBV32
  ITE :: (Typeable a, TBitRepr a) => Expr TBool -> Expr a -> Expr a -> Expr a
  ETMAYBE :: Int -> Expr TMaybeC
  EMAYBE_JUST :: (Typeable a, TBitRepr a) => Expr (TMaybe (a :: Type)) -> Expr (a :: Type) -- get the value inside Just
  EMAYBE_JUSTC :: (Typeable a, TBitRepr a) => Expr (a :: Type) -> Expr (TMaybe a) -- create a Just instance
  EMAYBE_C :: (Typeable a, TBitRepr a) => Expr (TMaybe a) -> Expr TMaybeC -- get the union type
  MKTUP :: (Typeable a, TBitRepr a, Typeable b, TBitRepr b) => Expr (a :: Type) -> Expr (b :: Type) -> Expr (TTuple a b)
  FST :: (Typeable a, TBitRepr a, Typeable b, TBitRepr b) => Expr (TTuple a b) -> Expr (a :: Type)
  SND :: (Typeable a, TBitRepr a, Typeable b, TBitRepr b) => Expr (TTuple a b) -> Expr (b :: Type)
  ELEM :: (Typeable a, TBitRepr a, Finite a) => Expr (a :: Type) -> Expr (TSet (a :: Type)) -> Expr TBool
  FUN0 :: (Typeable a, TBitRepr a, TBitRepr b) => String -> Expr (a :: Type) -> Expr (b :: Type)
  FUN1 :: (Typeable a, TBitRepr a, Typeable b, TBitRepr b, Typeable c, TBitRepr c) => String -> Expr (a :: Type) -> Expr (b :: Type) -> Expr (c :: Type)
  FUN2 ::
    (Typeable a, TBitRepr a, Typeable b, TBitRepr b, Typeable c, TBitRepr c, Typeable d, TBitRepr d) =>
    String ->
    Expr (a :: Type) ->
    Expr (b :: Type) ->
    Expr (c :: Type) ->
    Expr (d :: Type)
  FUN3 ::
    (Typeable a, TBitRepr a, Typeable b, TBitRepr b, Typeable c, TBitRepr c, Typeable d, TBitRepr d, Typeable e, TBitRepr e) =>
    String ->
    Expr (a :: Type) ->
    Expr (b :: Type) ->
    Expr (c :: Type) ->
    Expr (d :: Type) ->
    Expr (e :: Type)
  FUN4 ::
    (Typeable a, TBitRepr a, Typeable b, TBitRepr b, Typeable c, TBitRepr c, Typeable d, TBitRepr d, Typeable e, TBitRepr e, Typeable f, TBitRepr f) =>
    String ->
    Expr (a :: Type) ->
    Expr (b :: Type) ->
    Expr (c :: Type) ->
    Expr (d :: Type) ->
    Expr (e :: Type) ->
    Expr (f :: Type)
  CONJ :: [Expr TBool] -> Expr TBool
  -- FUN2 :: (Typeable a, TBitRepr a, TBitRepr b, TBitRepr c) => String -> Expr (a :: Type) ->  Expr (b :: Type) -> Expr (c :: Type)
  deriving (Typeable)

caseMaybeTBV32 :: (Typeable a, TBitRepr a) => Expr (TMaybe TBV32) -> (Expr TBV32 -> Expr a) -> Expr a -> Expr a
caseMaybeTBV32 v just nothing = ITE (EMAYBE_C v `EQU` constructorMaybeJust) (just (EMAYBE_JUST v)) nothing

{- caseNextRecv :: Expr TNextRecv -> (Expr TSetCh -> Expr a) -> Expr TChMsgInfo -> Expr a -> Expr a
caseNextRecv v noti ppcall unk = undefined -}

bitsize :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> Int
bitsize x = tbvsz @t

name :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> String
name _ = tname @t

smtlib :: forall t. Typeable t => Expr (t :: Type) -> SExpr Atom
smtlib (VAR s) = A (AVar s)
smtlib (EBV32 n) = A (AInt 32 n) ::: Nil
smtlib (EQU lhs rhs) = A AEq ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (EADD lhs rhs) = A AAdd ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ITE e lhs rhs) = A AITE ::: smtlib e ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ETMAYBE c) = A (AInt (tbvsz @TMaybeC) c) ::: Nil
smtlib (EMAYBE_JUST of_) = A (AExtract (tbvsz @TMaybeC) (bitsize of_)) ::: smtlib of_ ::: Nil
smtlib (EMAYBE_JUSTC of_) = A AConcat ::: L [smtlib constructorMaybeJust, smtlib of_] ::: Nil
smtlib (EMAYBE_C c) = A (AExtract 0 (tbvsz @TMaybeC)) ::: smtlib c ::: Nil
smtlib (MKTUP a b) = A AConcat ::: smtlib a ::: smtlib b ::: Nil
smtlib (FST a) = A (AExtract 0 (tbvsz @t)) ::: Nil
smtlib (SND a) = A (AExtract 0 (bitsize a)) ::: Nil
smtlib (CONJ ts) = A AConj ::: L (map smtlib ts) ::: Nil
smtlib (ELEM el set) = undefined
smtlib (FUN0 fname body) = A ADefineFunc ::: A (AAtom fname) ::: L [L []] ::: smtlib body ::: Nil
smtlib (FUN1 fname arg1 body) = A ADefineFunc ::: A (AAtom fname) ::: L [L [smtlib arg1, A (AAtom (name arg1))]] ::: smtlib body ::: Nil
smtlib (FUN2 fname arg1 arg2 body) = A ADefineFunc ::: A (AAtom fname) ::: L [L [smtlib arg1, A (AAtom (name arg1))], L [smtlib arg2, A (AAtom (name arg2))]] ::: smtlib body ::: Nil
smtlib (FUN3 fname arg1 arg2 arg3 body) =
  A ADefineFunc
    ::: A (AAtom fname)
    ::: L
      [ L [smtlib arg1, A (AAtom (name arg1))],
        L [smtlib arg2, A (AAtom (name arg2))],
        L [smtlib arg3, A (AAtom (name arg3))]
      ]
    ::: smtlib body
    ::: Nil
smtlib (FUN4 fname arg1 arg2 arg3 arg4 body) =
  A ADefineFunc
    ::: A (AAtom fname)
    ::: L
      [ L [smtlib arg1, A (AAtom (name arg1))],
        L [smtlib arg2, A (AAtom (name arg2))],
        L [smtlib arg3, A (AAtom (name arg3))],
        L [smtlib arg4, A (AAtom (name arg4))]
      ]
    ::: smtlib body
    ::: Nil

toSExpr :: (Typeable t) => Expr t -> SExpr Atom
toSExpr = smtlib

mkLangPrinter :: (Typeable t) => SExprPrinter Atom (Expr t)
mkLangPrinter =
  setFromCarrier toSExpr $
    setIndentStrategy (const Align) $
      basicPrint sAtom

printSMT :: (Typeable t) => Expr t -> T.Text
printSMT e = encode mkLangPrinter [e]

class SMTAdd a where
  (|+|) :: a -> a -> a

instance SMTAdd (Expr TBV32) where
  (|+|) lhs rhs = EADD lhs rhs

class BiMap a b c d where
  bimap :: a -> (b -> d) -> (c -> d) -> d

class Functor' a b where
  fmap' :: Expr a -> (Expr a -> b) -> b
  (<%>) :: Expr a -> (Expr a -> b) -> b
  default (<%>) :: Expr a -> (Expr a -> b) -> b
  (<%>) = fmap'

type family ASSMT a where
  ASSMT (Maybe a) = TMaybe (ASSMT a)
  ASSMT Int = TBV32

translate :: Exp a -> Expr (ASSMT a)
translate x = undefined -- left as an exercse to the reader
