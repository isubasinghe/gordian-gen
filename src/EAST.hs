{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module EAST where

import           Data.Kind    ()
import           Data.Proxy
import           GHC.Generics

data MyMaybe a = MyNothing | MyJust !a
  deriving (Generic)


-- >>> :kind! (Rep (MyMaybe Int))
-- (Rep (MyMaybe Int)) :: * -> *
-- = M1
--     D
--     ('MetaData "MyMaybe" "Lib" "main" 'False)
--     (M1 C ('MetaCons "MyNothing" 'PrefixI 'False) U1
--      :+: M1
--            C
--            ('MetaCons "MyJust" 'PrefixI 'False)
--            (M1
--               S
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
--               (K1 R Int)))

data MyTriple = Blah | Blah2 | Blah3 | Blah4 | Blah5
  deriving stock (Generic)

data MySingle = Single deriving (Generic)

data MyMediumOne = A1 Int Int Int Bool
  deriving stock (Generic)
  deriving anyclass (Size)

data MyBigOne = B1 !Int !Int !MyMediumOne | B2 !Int !Bool
  deriving stock (Generic)
  deriving anyclass (Size)

instance Size MyTriple

instance Size MySingle

newtype UInt32 = UInt32 {uint32 :: Int}

newtype Int32 = Int32 {int32 :: Int}

class Size a where
  numberOfConstructors :: Proxy a -> Int
  default numberOfConstructors :: GSize (Rep a) => Proxy a -> Int
  numberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy (Rep a))

  maxSize :: Proxy a -> Int
  default maxSize :: GSize (Rep a) => Proxy a -> Int
  maxSize _ = gmaxSize (Proxy :: Proxy (Rep a))

  bitvecSize :: Proxy a -> Int
  default bitvecSize :: GSize (Rep a) => Proxy a -> Int
  bitvecSize _ = gbitvecSize (Proxy :: Proxy (Rep a))

  smtName :: Proxy a -> String
  default smtName :: GSize (Rep a) => Proxy a -> String
  smtName _ = gsmtName (Proxy :: Proxy (Rep a))

-- Define the GSize type class and its instances
class GSize (f :: k) where
  gnumberOfConstructors :: Proxy f -> Int
  gmaxSize :: Proxy f -> Int
  gbitvecSize :: Proxy f -> Int
  gsmtName :: Proxy f -> String

instance {-# OVERLAPPABLE #-} GSize f => GSize (M1 i c f) where
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy f)
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = 0
  gsmtName _ = ""

instance (GSize f) => GSize (M1 D c f) where
  gnumberOfConstructors _ = 1
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = gmaxSize (Proxy :: Proxy f)
  gsmtName _ = ""

instance (GSize f) => GSize (M1 C c f) where
  gnumberOfConstructors _ = 1
  gmaxSize _ = gmaxSize (Proxy :: Proxy f)
  gbitvecSize _ = 0
  gsmtName _ = ""

instance (GSize a, GSize b) => GSize (a :*: b) where
  gnumberOfConstructors _ = 0
  gmaxSize _ = gmaxSize (Proxy :: Proxy a) + gmaxSize (Proxy :: Proxy b)
  gbitvecSize _ = 0
  gsmtName _ = ""

instance (GSize a, GSize b) => GSize (a :+: b) where
  gnumberOfConstructors _ = gnumberOfConstructors (Proxy :: Proxy a) + gnumberOfConstructors (Proxy :: Proxy b)
  gmaxSize _ = max (gmaxSize (Proxy :: Proxy a)) (gmaxSize (Proxy :: Proxy b))
  gbitvecSize _ = 0
  gsmtName _ = ""

instance GSize U1 where
  gnumberOfConstructors _ = 0
  gmaxSize _ = 0
  gbitvecSize _ = 0
  gsmtName _ = ""

instance (Size a) => GSize (K1 i a) where
  gnumberOfConstructors _ = numberOfConstructors (Proxy :: Proxy a)
  gmaxSize _ = maxSize (Proxy :: Proxy a)
  gbitvecSize _ = 0
  gsmtName _ = ""

instance (Size a) => Size (MyMaybe a)

instance Size Int where
  numberOfConstructors _ = 0
  maxSize _ = 32
  bitvecSize _ = 32
  smtName _ = ""

instance Size Bool where
  numberOfConstructors _ = 0
  maxSize _ = 1
  bitvecSize _ = 1
  smtName _ = ""




myMaybePre :: MyMaybe Int -> Bool
myMaybePre m = case m of
                MyJust a  -> a == 4
                MyNothing -> False

-- (MyMaybeInt)
-- smtName (Proxy :: (Proxy (MyMaybe Int)))
-- smtName (MyJust Int)

-- function(m)
-- (ite ((type m) == MyJust) (= (MyJustInt.Get m) 4) False)


data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

instance Show (HList '[]) where
  show HNil = "HNil"

instance
  (Show (HList as), Show a) =>
  Show (HList (a ': as))
  where
  show (a ::: rest) = show a ++ " ::: " ++ show rest

infixr 6 :::

{- data Expr a where
  Add :: Expr Int -> Expr Int -> Expr Int
  Cases :: HList xs -> Expr as
  ConstInt :: Int -> Expr Int
  ConstSum :: HList xs -> Expr (HList xs) -}

data Void

{- myMaybeIntSum :: Expr (HList '[Proxy Int, Proxy Int])
myMaybeIntSum =  ConstSum $ (Proxy :: Proxy Int) ::: (Proxy :: Proxy Int) ::: HNil -}

type family Proxied n where
  Proxied n = n

type family Apply n m where
  Apply n m = n

data SimpleGADT a where
  SG1 :: SimpleGADT Int
deriving instance Show (SimpleGADT a)

funcName :: SimpleGADT a -> String
funcName  x = case x of
                    SG1 -> show x

