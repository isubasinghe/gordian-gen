{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module EAST where

import Data.Kind ()
import Data.Proxy (Proxy (..))
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
  gbitvecSize _ = 0
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
  gconstructorSizes _ = [Just $ gbitvecSize (Proxy :: Proxy f)]

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
  gbitvecSize _ = error "should not be reached"
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
funcName x = case x of
  SG1 -> show x
