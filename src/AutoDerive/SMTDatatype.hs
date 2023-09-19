{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module AutoDerive.SMTDatatype where

import Data.Proxy (Proxy (..))
import GHC.Float (int2Float)
import GHC.Generics


data SMTType
  =  SMTInt
  | SMTDatatype


data SMTDataType = SMTDataType String


class SMTRepr a where
  smtName :: Proxy a -> String
  encode :: Proxy a -> SMTDataType

class GSMTRepr (f :: k) where
  gsmtName :: Proxy f -> String

instance {-# OVERLAPPABLE #-} GSMTRepr f => GSMTRepr (M1 i c f) where
  gsmtName _ = gsmtName (Proxy :: Proxy f)

instance (GSMTRepr f) => GSMTRepr (M1 D c f) where
  gsmtName _ = gsmtName (Proxy :: Proxy f)

instance (GSMTRepr f) => GSMTRepr (M1 C c f) where
  gsmtName _ = gsmtName (Proxy :: Proxy f)
