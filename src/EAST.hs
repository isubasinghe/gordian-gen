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
import Data.Word
import AutoDerive.BitVecRepr
import EDSL.Bool ()
import EDSL.Elt
import EDSL.Exp
import EDSL.Maybe ()
import EDSL.Rec
import EDSL.Trace
import EDSL.Tuple
import EDSL.Type
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
  tname = "Bool"

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
  tbvsz = 8
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
  RAW :: (Typeable a, TBitRepr a) => SExpr Atom -> Expr a
  VAR :: (Typeable a, TBitRepr a) => String -> Expr a
  EBOOL :: Bool -> Expr TBool
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
caseNextRecv v noti ppcall unk = error "caseNextRecv: not implemented" -}

bitsize :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> Int
bitsize x = tbvsz @t

name :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> String
name _ = tname @t

sortName :: forall t. TBitRepr t => String
sortName
  | tname @t == "Bool" = "Bool"
  | otherwise = "(_ BitVec " ++ show (tbvsz @t) ++ ")"

sortExpr :: forall t. TBitRepr t => SExpr Atom
sortExpr = A (AAtom (sortName @t))

extract :: Int -> Int -> SExpr Atom -> SExpr Atom
extract hi lo e = A (AExtract hi lo) ::: e ::: Nil

concatExpr :: SExpr Atom -> SExpr Atom -> SExpr Atom
concatExpr a b = A AConcat ::: a ::: b ::: Nil

smtlib :: forall t. Typeable t => Expr (t :: Type) -> SExpr Atom
smtlib (RAW e) = e
smtlib (VAR s) = A (AVar s)
smtlib (EBOOL True) = A (AAtom "true")
smtlib (EBOOL False) = A (AAtom "false")
smtlib (EBV32 n) = A (AInt 32 n)
smtlib (EQU lhs rhs) = A AEq ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (EADD lhs rhs) = A AAdd ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ITE e lhs rhs) = A AITE ::: smtlib e ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ETMAYBE c) = A (AInt (tbvsz @TMaybeC) c)
smtlib (EMAYBE_JUST (of_ :: Expr (TMaybe a))) = extract (tbvsz @a - 1) 0 (smtlib of_)
smtlib (EMAYBE_JUSTC of_) = concatExpr (smtlib constructorMaybeJust) (smtlib of_)
smtlib (EMAYBE_C c) = extract (bitsize c - 1) (bitsize c - tbvsz @TMaybeC) (smtlib c)
smtlib (MKTUP a b) = A AConcat ::: smtlib a ::: smtlib b ::: Nil
smtlib (FST (a :: Expr (TTuple l r))) = extract (bitsize a - 1) (tbvsz @r) (smtlib a)
smtlib (SND (a :: Expr (TTuple l r))) = extract (tbvsz @r - 1) 0 (smtlib a)
smtlib (CONJ []) = A (AAtom "true")
smtlib (CONJ [t]) = smtlib t
smtlib (CONJ ts) = L (A AConj : map smtlib ts)
smtlib (ELEM el set) =
  A (AAtom "select")
    ::: smtlib set
    ::: smtlib el
    ::: Nil
smtlib (FUN0 fname body) = A ADefineFunc ::: A (AAtom fname) ::: L [] ::: sortExpr @t ::: smtlib body ::: Nil
smtlib (FUN1 fname arg1 body) = A ADefineFunc ::: A (AAtom fname) ::: L [binder arg1] ::: sortExpr @t ::: smtlib body ::: Nil
smtlib (FUN2 fname arg1 arg2 body) = A ADefineFunc ::: A (AAtom fname) ::: L [binder arg1, binder arg2] ::: sortExpr @t ::: smtlib body ::: Nil
smtlib (FUN3 fname arg1 arg2 arg3 body) =
  A ADefineFunc
    ::: A (AAtom fname)
    ::: L
      [ binder arg1,
        binder arg2,
        binder arg3
      ]
    ::: sortExpr @t
    ::: smtlib body
    ::: Nil
smtlib (FUN4 fname arg1 arg2 arg3 arg4 body) =
  A ADefineFunc
    ::: A (AAtom fname)
    ::: L
      [ binder arg1,
        binder arg2,
        binder arg3,
        binder arg4
      ]
    ::: sortExpr @t
    ::: smtlib body
    ::: Nil

binder :: forall a. (Typeable a, TBitRepr a) => Expr a -> SExpr Atom
binder (VAR s) = L [A (AAtom s), sortExpr @a]
binder _ = error "SMT function arguments must be variables"

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
  ASSMT (a, b) = TTuple (ASSMT a) (ASSMT b)
  ASSMT Bool = TBool
  ASSMT Int = TBV32
  ASSMT Word8 = TCh

class (Elt a, BitVecRepr a, Typeable (ASSMT a), TBitRepr (ASSMT a)) => SMTTranslate a where
  translate :: Exp a -> Expr (ASSMT a)

instance SMTTranslate Int where
  translate (Const n) = EBV32 n
  translate (Var (Idx v)) = VAR (T.unpack v)
  translate (Case x xs) = translateCase x xs
  translate e = RAW (rawSExpr (rawExp e))

instance SMTTranslate Bool where
  translate (Const b) = EBOOL (toElt b)
  translate (Var (Idx v)) = VAR (T.unpack v)
  translate (Eq x y) = rawEq (rawExp x) (rawExp y)
  translate (Case x xs) = translateCase x xs
  translate (Tuple t) = rawBool (rawTuple t)
  translate e = rawBool (rawExp e)

instance (SMTTranslate a, Typeable (ASSMT a), TBitRepr (ASSMT a)) => SMTTranslate (Maybe a) where
  translate e = RAW (rawSExpr (rawExp e))

instance
  ( SMTTranslate a,
    SMTTranslate b,
    Typeable (ASSMT a),
    Typeable (ASSMT b),
    TBitRepr (ASSMT a),
    TBitRepr (ASSMT b)
  ) =>
  SMTTranslate (a, b)
  where
  translate e = RAW (rawSExpr (rawExp e))

data Raw = Raw
  { rawWidth :: Int,
    rawSExpr :: SExpr Atom
  }

rawExp :: forall a. (Elt a, BitVecRepr a) => Exp a -> Raw
rawExp (Const c) = rawValue (eltR @a) c
rawExp (Var (Idx v)) = Raw (bitvecSize (Proxy @a)) (A (AVar (T.unpack v)))
rawExp (Tuple t) = rawTuple t
rawExp (Prj ix t) = projectRaw ix (rawExp t)
rawExp (Match _ e) = rawExp e
rawExp (Case x xs) = rawCase x xs
rawExp (Undef t) = rawZero (bitvecSizeOfTypeR t)
rawExp (Eq x y) = Raw 1 (smtlib (rawEq (rawExp x) (rawExp y)))
rawExp _ = error "translate: unsupported EDSL expression in SMT backend"

rawTuple :: Tuple t -> Raw
rawTuple Unit = Raw 0 (A (AAtom ""))
rawTuple (Exp e) = rawExp e
rawTuple (Pair a b) = rawConcat (rawTuple a) (rawTuple b)

rawValue :: TypeR a -> a -> Raw
rawValue TypeRunit () = Raw 0 (A (AAtom ""))
rawValue (TypeRprim t) v = rawPrim t v
rawValue (TypeRpair a b) (x, y) = rawConcat (rawValue a x) (rawValue b y)
rawValue (TypeRrec t) (Rec x) = rawValue t (fromElt x)

rawPrim :: PrimType a -> a -> Raw
rawPrim (IntegralNumType t) v = rawIntegral t v
rawPrim (FloatingNumType _) _ = error "translate: floating point values are not supported"

rawIntegral :: IntegralType a -> a -> Raw
rawIntegral TypeInt n = Raw (bitvecSize (Proxy @Int)) (A (AInt (bitvecSize (Proxy @Int)) n))
rawIntegral TypeInteger n = Raw (bitvecSize (Proxy @Integer)) (A (AInt (bitvecSize (Proxy @Integer)) (fromInteger n)))
rawIntegral TypeWord8 n = Raw (bitvecSize (Proxy @Word8)) (A (AInt (bitvecSize (Proxy @Word8)) (fromIntegral n)))

rawZero :: Int -> Raw
rawZero w = Raw w (A (AInt w 0))

rawConcat :: Raw -> Raw -> Raw
rawConcat (Raw 0 _) r = r
rawConcat l (Raw 0 _) = l
rawConcat l r = Raw (rawWidth l + rawWidth r) (concatExpr (rawSExpr l) (rawSExpr r))

projectRaw :: forall s e. ReprWidth e => TupleIdx s e -> Raw -> Raw
projectRaw PrjZ raw = Raw (reprWidth (Proxy @e)) (rawSExpr raw)
projectRaw (PrjL (ix :: TupleIdx l e)) raw =
  let leftWidth = reprWidth (Proxy @l)
      rightWidth = rawWidth raw - leftWidth
      leftRaw
        | rightWidth == 0 = Raw leftWidth (rawSExpr raw)
        | otherwise = Raw leftWidth (extract (rawWidth raw - 1) rightWidth (rawSExpr raw))
   in projectRaw ix leftRaw
projectRaw (PrjR (ix :: TupleIdx r e)) raw =
  let rightWidth = reprWidth (Proxy @r)
      rightRaw
        | rightWidth == rawWidth raw = Raw rightWidth (rawSExpr raw)
        | otherwise = Raw rightWidth (extract (rightWidth - 1) 0 (rawSExpr raw))
   in projectRaw ix rightRaw

rawEq :: Raw -> Raw -> Expr TBool
rawEq a b = RAW (A AEq ::: rawSExpr a ::: rawSExpr b ::: Nil)

rawBool :: Raw -> Expr TBool
rawBool (Raw 1 e) = RAW (A AEq ::: e ::: A (AInt 1 1) ::: Nil)
rawBool r = RAW (A AEq ::: rawSExpr r ::: A (AInt (rawWidth r) 1) ::: Nil)

rawCase :: (Elt a, BitVecRepr a, Elt b, BitVecRepr b) => Exp a -> [(TraceR (EltR a), Exp b)] -> Raw
rawCase _ [] = error "translate: empty case"
rawCase _ [(_, r)] = rawExp r
rawCase x ((tr, r) : rest) =
  let thenRaw = rawExp r
      elseRaw = rawCase x rest
   in Raw
        (rawWidth thenRaw)
        (A AITE ::: smtlib (traceMatch tr x) ::: rawSExpr thenRaw ::: rawSExpr elseRaw ::: Nil)

translateCase :: (Elt a, BitVecRepr a, SMTTranslate b) => Exp a -> [(TraceR (EltR a), Exp b)] -> Expr (ASSMT b)
translateCase _ [] = error "translate: empty case"
translateCase _ [(_, r)] = translate r
translateCase x ((tr, r) : rest) = ITE (traceMatch tr x) (translate r) (translateCase x rest)

traceMatch :: (Elt a, BitVecRepr a) => TraceR (EltR a) -> Exp a -> Expr TBool
traceMatch tr e = traceMatchRaw tr (rawExp e)

traceMatchRaw :: TraceR a -> Raw -> Expr TBool
traceMatchRaw TraceRunit _ = EBOOL True
traceMatchRaw (TraceRprim _) _ = EBOOL True
traceMatchRaw (TraceRundef _) _ = EBOOL True
traceMatchRaw (TraceRrec _) _ = EBOOL True
traceMatchRaw (TraceRtag tag tr) raw =
  conjExpr
    [ rawEq tagRaw (Raw 8 (A (AInt 8 (fromIntegral tag)))),
      traceMatchRaw tr restRaw
    ]
  where
    restWidth = bitvecSizeOfTraceR tr
    tagRaw = Raw 8 (extract (rawWidth raw - 1) restWidth (rawSExpr raw))
    restRaw = Raw restWidth (extract (restWidth - 1) 0 (rawSExpr raw))
traceMatchRaw (TraceRpair a b) raw =
  conjExpr [traceMatchRaw a leftRaw, traceMatchRaw b rightRaw]
  where
    rightWidth = bitvecSizeOfTraceR b
    leftWidth = bitvecSizeOfTraceR a
    leftRaw = Raw leftWidth (extract (rawWidth raw - 1) rightWidth (rawSExpr raw))
    rightRaw = Raw rightWidth (extract (rightWidth - 1) 0 (rawSExpr raw))

conjExpr :: [Expr TBool] -> Expr TBool
conjExpr xs =
  case filter (not . isTrue) xs of
    [] -> EBOOL True
    [x] -> x
    ys -> CONJ ys
  where
    isTrue (EBOOL True) = True
    isTrue _ = False
