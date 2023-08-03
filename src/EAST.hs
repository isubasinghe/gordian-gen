{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module EAST where
import GHC.Generics

import           Data.Char
import           Data.List
import           Data.Typeable

class GSize f where
  gsize :: f p -> Int

instance (GSize a, GSize b) => GSize (a :*: b) where
  gsize (_ :*: _) = gsize (undefined :: a p) + gsize (undefined :: b p)

instance (GSize a) => GSize (M1 D c a) where
  gsize (M1 x) = gsize x

instance (GSize a) => GSize (M1 C c a) where
  gsize (M1 x) = gsize x

instance (GSize a) => GSize (M1 S c a) where
  gsize (M1 x) = gsize x

instance (GSize a) => GSize (M1 i c a) where
  gsize (M1 x) = gsize x

instance BVBoolRepr a => GSize (K1 R a) where
  gsize (K1 x) = size x

class BVBoolRepr a where
  size :: a -> Int

  default size :: (Generic a, GSize (Rep a)) => a -> Int
  size = gsize . from

instance BVBoolRepr Bool where
  size _ = 1

instance BVBoolRepr Int where
  size _ = 32  -- Assuming a 32-bit integer
