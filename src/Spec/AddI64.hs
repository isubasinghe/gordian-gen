{-# LANGUAGE DataKinds #-}

module Spec.AddI64 where

import qualified Data.Text as T
import EAST
import SMT.DSL

addI64Pre :: I64 -> I64 -> SBool
addI64Pre = noSignedOverflowAdd64

addI64Post :: I64 -> I64 -> I64 -> SBool
addI64Post = signedAdd64Spec

xI64 :: I64
xI64 = var "x"

yI64 :: I64
yI64 = var "y"

resultI64 :: I64
resultI64 = var "result"

addI64PreSMT :: T.Text
addI64PreSMT = printSMT (addI64Pre xI64 yI64)

addI64PostSMT :: T.Text
addI64PostSMT = printSMT (addI64Post xI64 yI64 resultI64)
