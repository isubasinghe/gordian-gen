{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Text.Internal.Fusion.Types (RS (RS0))
import Data.Typeable
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
import Text.Read (Lexeme(String))

data MyMaybe a = MyNothing | MyJust !a
  deriving (Generic)

-- >>> :kind! (Rep (MyMaybe Int))
-- (Rep (MyMaybe Int)) :: * -> *
-- = D1
--     ('MetaData "MyMaybe" "EAST" "main" 'False)
--     (C1 ('MetaCons "MyNothing" 'PrefixI 'False) U1
--      :+: C1
--            ('MetaCons "MyJust" 'PrefixI 'False)
--            (S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Int)))
--
-- MyMaybe Int
-- numberOfConstructors = 2
-- maxSize = 32
-- bitvecSize = 33
-- smtName = "MyMaybe_of_Int"
-- constructors = [Just "MyJust_of_Int", Just "Nothing"]
-- constructorSizes = [Just 32, Nothing]

-- >>> :kind! (Rep (MyMaybe (MyMaybe Int)))
-- (Rep (MyMaybe (MyMaybe Int))) :: * -> *
-- = D1
--     ('MetaData "MyMaybe" "EAST" "main" 'False)
--     (C1 ('MetaCons "MyNothing" 'PrefixI 'False) U1
--      :+: C1
--            ('MetaCons "MyJust" 'PrefixI 'False)
--            (S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 (MyMaybe Int))))

data MyEither a b = MyLeft !a | MyRight !b !b
  deriving stock (Generic)
  deriving anyclass (BitVecRepr)

-- >>> :kind! (Rep (MyEither Int Int))
-- (Rep (MyEither Int Int)) :: * -> *
-- = D1
--     ('MetaData "MyEither" "EAST" "main" 'False)
--     (C1
--        ('MetaCons "MyLeft" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 Int))
--      :+: C1
--            ('MetaCons "MyRight" 'PrefixI 'False)
--            (S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Int)
--             :*: S1
--                   ('MetaSel
--                      'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                   (Rec0 Int)))

data MyTriple = Blah | Blah2 | Blah3 | Blah4 | Blah5
  deriving stock (Generic)

data MySingle = Single deriving (Generic)

data MyMediumOne = A1 !Int !Int !Int !Bool
  deriving stock (Generic)
  deriving anyclass (BitVecRepr)

-- >>> :kind! (Rep MyMediumOne)
-- (Rep MyMediumOne) :: * -> *
-- = D1
--     ('MetaData "MyMediumOne" "EAST" "main" 'False)
--     (C1
--        ('MetaCons "A1" 'PrefixI 'False)
--        ((S1
--            ('MetaSel
--               'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--            (Rec0 Int)
--          :*: S1
--                ('MetaSel
--                   'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                (Rec0 Int))
--         :*: (S1
--                ('MetaSel
--                   'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                (Rec0 Int)
--              :*: S1
--                    ('MetaSel
--                       'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--                    (Rec0 Bool))))

data MyManyCons a b = MMC !a | MMC1 !b
  deriving stock (Generic)
  deriving anyclass (BitVecRepr)

-- >>> :kind! (Rep (MyManyCons Int Bool))
-- (Rep (MyManyCons Int Bool)) :: * -> *
-- = D1
--     ('MetaData "MyManyCons" "EAST" "main" 'False)
--     (C1
--        ('MetaCons "MMC" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 Int))
--      :+: C1
--            ('MetaCons "MMC1" 'PrefixI 'False)
--            (S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Bool)))
--

data MyBigOne = B1 !Int !Int !MyMediumOne | B2 !Int !Bool
  deriving stock (Generic)
  deriving anyclass (BitVecRepr)

instance BitVecRepr MyTriple

instance BitVecRepr MySingle

newtype UInt32 = UInt32 {uint32 :: Int}

newtype Int32 = Int32 {int32 :: Int}

class BitVecRepr a where
  numberOfConstructors :: Proxy a -> Int
  default numberOfConstructors :: GBitVecRepr (Rep a) => Proxy a -> Int
  numberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy (Rep a))

  maxSize :: Proxy a -> Int
  default maxSize :: GBitVecRepr (Rep a) => Proxy a -> Int
  maxSize _ = gmaxSize (Proxy :: Proxy (Rep a))

  bitvecSize :: Proxy a -> Int
  default bitvecSize :: GBitVecRepr (Rep a) => Proxy a -> Int
  bitvecSize _ = gbitvecSize (Proxy :: Proxy (Rep a))

  smtName :: Proxy a -> String
  default smtName :: GBitVecRepr (Rep a) => Proxy a -> String
  smtName _ = gsmtName (Proxy :: Proxy (Rep a))

  constructorSizes :: Proxy a -> [Maybe Int]
  default constructorSizes :: GBitVecRepr (Rep a) => Proxy a -> [Maybe Int]
  constructorSizes _ = gconstructorSizes (Proxy :: Proxy (Rep a))

  constructors :: Proxy a -> [Maybe String]
  default constructors :: GBitVecRepr (Rep a) => Proxy a -> [Maybe String]
  constructors _ = gconstructors (Proxy :: Proxy (Rep a))

-- Define the GSize type class and its instances
class GBitVecRepr (f :: k) where
  gnumberOfConstructors :: Proxy f -> Int
  gmaxSize :: Proxy f -> Int
  gbitvecSize :: Proxy f -> Int
  gsmtName :: Proxy f -> String

  -- List of constructor sizes
  gconstructorSizes :: Proxy f -> [Maybe Int]
  gconstructors :: Proxy f -> [Maybe String]

instance {-# OVERLAPPABLE #-} GBitVecRepr f => GBitVecRepr (M1 i c f) where
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy f)
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = gbitvecSize (Proxy :: Proxy f)
  gsmtName _ = gsmtName (Proxy :: Proxy f)
  gconstructorSizes _ = gconstructorSizes (Proxy :: Proxy f)
  gconstructors _ = gconstructors (Proxy :: Proxy f)

instance (GBitVecRepr f, Datatype c) => GBitVecRepr (M1 D c f) where
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy f)
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = gmaxSize (Proxy :: Proxy f) + fromCons
    where
      fromCons :: Int
      fromCons = if numCons /= 1 then (+ (numCons `mod` 2)) . floor $ logBase 2 (int2Float numCons) else 0
      numCons = gnumberOfConstructors (Proxy :: Proxy f)

  gsmtName _ = datatypeName (undefined :: M1 D c f x) ++ (if rest /= "" then "_of_" ++ rest else "")
    where
      rest = gsmtName (Proxy :: Proxy f)
  gconstructorSizes _ = gconstructorSizes (Proxy :: Proxy f)
  gconstructors _ = gconstructors (Proxy :: Proxy f)

instance (GBitVecRepr f, Constructor c) => GBitVecRepr (M1 C c f) where
  gnumberOfConstructors _ = 1
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = error "should not be reached"
  gsmtName _ = gsmtName (Proxy :: Proxy f)
  gconstructors _ = [Just (currName ++ rest)]
    where
      currName = conName (undefined :: M1 C c f x)
      sname = gsmtName (Proxy :: Proxy f)
      rest = if sname /= "" then "_of_" ++ sname else ""
  gconstructorSizes _ = [if bvsize /= 0 then Just bvsize else Nothing]
    where
      bvsize = gbitvecSize (Proxy :: Proxy f)

instance (GBitVecRepr a, GBitVecRepr b) => GBitVecRepr (a :*: b) where
  gnumberOfConstructors _ = 0
  gmaxSize _ = gmaxSize (Proxy :: Proxy a) + gmaxSize (Proxy :: Proxy b)
  gbitvecSize _ = error "should not be reached"
  gsmtName _ = left ++ "_prod_" ++ right
    where
      left = gsmtName (Proxy :: Proxy a)
      right = gsmtName (Proxy :: Proxy b)
  gconstructors _ = undefined
  gconstructorSizes _ = undefined

instance (GBitVecRepr a, GBitVecRepr b) => GBitVecRepr (a :+: b) where
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy a) + gnumberOfConstructors (Proxy :: Proxy b)
  gmaxSize _ = max (gmaxSize (Proxy :: Proxy a)) (gmaxSize (Proxy :: Proxy b))
  gbitvecSize _ = error "should not be reached"
  gsmtName _ = left ++ (if left /= "" && (right /= "") then "_sum_" else "") ++ right
    where
      left = gsmtName (Proxy :: Proxy a)
      right = gsmtName (Proxy :: Proxy b)
  gconstructors _ = gconstructors (Proxy :: Proxy a) ++ gconstructors (Proxy :: Proxy b)
  gconstructorSizes _ = gconstructorSizes (Proxy :: Proxy a) ++ gconstructorSizes (Proxy :: Proxy b)

instance GBitVecRepr U1 where
  gnumberOfConstructors _ = 0
  gmaxSize _ = 0
  gbitvecSize _ = 0
  gsmtName _ = ""
  gconstructors _ = undefined
  gconstructorSizes _ = undefined

instance (BitVecRepr a) => GBitVecRepr (K1 i a) where
  gnumberOfConstructors _ = numberOfConstructors (Proxy :: Proxy a)
  gmaxSize _ = maxSize (Proxy :: Proxy a)
  gbitvecSize _ = bitvecSize (Proxy :: Proxy a)
  gsmtName _ = smtName (Proxy :: Proxy a)
  gconstructorSizes _ = []
  gconstructors _ = []

instance (BitVecRepr a) => BitVecRepr (MyMaybe a)

instance BitVecRepr Int where
  numberOfConstructors _ = 0
  maxSize _ = 32
  bitvecSize _ = 32
  smtName _ = "Int"
  constructors _ = []
  constructorSizes _ = []

instance BitVecRepr Bool where
  numberOfConstructors _ = 0
  maxSize _ = 1
  bitvecSize _ = 1
  smtName _ = "Bool"
  constructors _ = [Just "bvtrue", Just "bvfalse"]
  constructorSizes _ = [Just 1, Just 1]

myMaybePre :: MyMaybe Int -> Bool
myMaybePre m = case m of
  MyJust a -> a == 4
  MyNothing -> False

-- (MyMaybeInt)
-- smtName (Proxy :: (Proxy (MyMaybe Int)))
-- smtName (MyJust Int)

-- function(m)
-- (ite ((type m) == MyJust) (= (MyJustInt.Get m) 4) False)

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)

data HList :: [*] -> * where
  HNil :: HList '[]
  (:#:) :: a -> HList as -> HList (a ': as)

-- (:@:) :: HList as -> HList bs -> HList (as ++ bs)

instance Show (HList '[]) where
  show HNil = "HNil"

instance
  (Show (HList as), Show a) =>
  Show (HList (a ': as))
  where
  show (a :#: rest) = show a ++ " ::: " ++ show rest

infixr 6 :#:

data Void

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
  numElems = 2

data Type where
  TBV32 :: Type
  TBV64 :: Type
  TBool :: Type
  TCh :: Type
  TPD :: Type
  TChMsgInfo :: Type
  -- easier to monomorphise
  -- alternative is to use Proxy (Data.Proxy)
  -- to carry type level information across
  TSetCh :: Type
  TMsgInfo :: Type
  TMaybe :: Type -> Type
  TMaybeC :: Type
  TTuple :: Type -> Type -> Type
  deriving (Typeable, Data)

{- TNextRecv :: Type
TNextRecvC :: Type -}

class TBitRepr a where
  tbvsz :: Int
  tname :: String

instance TBitRepr 'TBV32 where
  tbvsz = 32
  tname = "BV32"

instance TBitRepr 'TBV64 where
  tbvsz = 64
  tname = "BV64"


instance TBitRepr 'TBool where
  tbvsz = 1
  tname = "BV1"

instance TBitRepr 'TCh where
  tbvsz = 8
  tname = "BV8"

instance TBitRepr 'TPD where
  tbvsz = 8
  tname = "BV8"

instance TBitRepr 'TChMsgInfo where
  tbvsz = (tbvsz @TCh) + (tbvsz @TMsgInfo)
  tname = "ChMsgInfo"

instance TBitRepr 'TSetCh where
  tbvsz = (tbvsz @TCh) * 8
  tname = "SetCh"


instance TBitRepr 'TMsgInfo where
  tbvsz = 32
  tname = "MsgInfo"

instance TBitRepr 'TMaybeC where
  tbvsz = 1
  tname = "MaybeC"

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
  FUN0 :: (Typeable a, TBitRepr a, TBitRepr b) => String -> Expr (a :: Type) -> Expr (b :: Type)
  FUN1 :: (Typeable a, TBitRepr a, Typeable b, TBitRepr b, Typeable c,TBitRepr c) => String -> Expr (a :: Type) ->  Expr (b :: Type) -> Expr (c :: Type)
  -- FUN2 :: (Typeable a, TBitRepr a, TBitRepr b, TBitRepr c) => String -> Expr (a :: Type) ->  Expr (b :: Type) -> Expr (c :: Type)
  deriving (Typeable)

caseMaybeTBV32 :: (Typeable a, TBitRepr a) => Expr (TMaybe TBV32) -> (Expr TBV32 -> Expr a) -> Expr a -> Expr a
caseMaybeTBV32 v just nothing = ITE (EMAYBE_C v `EQU` constructorMaybeJust) (just (EMAYBE_JUST v)) nothing

{- caseNextRecv :: Expr TNextRecv -> (Expr TSetCh -> Expr a) -> Expr TChMsgInfo -> Expr a -> Expr a
caseNextRecv v noti ppcall unk = undefined -}

pre :: Expr (TMaybe TBV32) -> Expr TBV32
pre maybe32 = caseMaybeTBV32 maybe32 (\x -> x `EADD` EBV32 1) (EBV32 0)

constMaybe :: Expr (TMaybe TBV32)
constMaybe = VAR "hello"

pre' :: Expr TBV32
pre' = pre constMaybe

asd :: Expr (TTuple TBV32 TBV64)
asd = VAR "asd"

funASD :: Expr (TTuple TBV32 TBV64)
funASD = FUN0 "nothingness" asd

fun1ASD :: Expr (TTuple TBV32 TBV64)
fun1ASD = FUN1 "nextfun" constMaybe asd

fstASD :: Expr TBV32
fstASD = FST asd
sndASD = SND asd

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

bitsize :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> Int
bitsize x = tbvsz @t

name :: forall t. (Typeable t, TBitRepr t) => Expr (t :: Type) -> String
name _ = tname @t


protectedWP :: Expr TCh -> Expr TMsgInfo -> Expr (TMaybe (TTuple TCh TMsgInfo))
protectedWP ch msg = undefined

smtlib :: forall t. Typeable t => Expr (t :: Type) -> SExpr Atom
smtlib (VAR s) = A (AVar s)
smtlib (EBV32 n) = A (AInt 32 n) ::: Nil
smtlib (EQU lhs rhs) = A AEq ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (EADD lhs rhs) = A AAdd ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ITE e lhs rhs) = A AITE ::: smtlib e ::: smtlib lhs ::: smtlib rhs ::: Nil
smtlib (ETMAYBE c) = A (AInt (tbvsz @TMaybeC) c) ::: Nil
smtlib (EMAYBE_JUST of_) = A (AExtract (tbvsz @TMaybeC) (bitsize of_)) ::: smtlib of_ ::: Nil
smtlib (EMAYBE_JUSTC of_) = A (AExtract (tbvsz @TMaybeC) (bitsize of_)) ::: smtlib of_ ::: Nil
smtlib (EMAYBE_C c) = A (AExtract 0 (tbvsz @TMaybeC)) ::: smtlib c ::: Nil
smtlib (MKTUP a b) = A AConcat ::: smtlib a ::: smtlib b ::: Nil
smtlib (FST a) = A (AExtract 0 (tbvsz @t)) ::: Nil
smtlib (SND a) = A (AExtract 0 (bitsize a)) ::: Nil
smtlib (FUN0 fname body) = A ADefineFunc ::: A (AAtom fname) ::: L [L []] ::: smtlib body ::: Nil
smtlib (FUN1 fname arg1 body) = A ADefineFunc ::: A (AAtom fname) ::: L [L [smtlib arg1, A (AAtom (name arg1))]] ::: smtlib body ::: Nil

data Atom
  = AAdd
  | AEq
  | AITE
  | AExtract !Int !Int
  | AConcat
  | AVar !String
  | ADefineFunc
  | AAtom !String
  | AInt !Int !Int -- bitvector with size then value

sAtom :: Atom -> T.Text
sAtom = \case
  AAdd -> "+"
  AEq -> "="
  AVar s -> T.pack s
  ADefineFunc -> "define-fun"
  AAtom s -> T.pack s
  AExtract n n' -> T.pack $ "extract " ++ show n ++ " " ++ show n'
  AITE -> T.pack "ite"
  AInt sz val -> T.pack $ "bv" ++ show val ++ " " ++ show sz
  AConcat -> "concat"

toSExpr :: (Typeable t) => Expr t -> SExpr Atom
toSExpr = smtlib

mkLangPrinter :: (Typeable t) => SExprPrinter Atom (Expr t)
mkLangPrinter =
  setFromCarrier toSExpr $
    setIndentStrategy (const Align) $
      basicPrint sAtom

printSMT :: (Typeable t) => Expr t -> T.Text
printSMT e = encode mkLangPrinter [e]
