{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT.DSL
  ( I64,
    SBool,
    var,
    lit64,
    add64,
    noSignedOverflowAdd64,
    signedAdd64Spec,
    (===),
    (.&&),
    (.||),
    not_,
    implies,
  )
where

import Data.Int
import Data.SCargot.Repr.Basic
import Data.Typeable
import EAST
import SMT

type I64 = Expr TBV64

type SBool = Expr TBool

var :: (Typeable t, TBitRepr t) => String -> Expr t
var = VAR

lit64 :: Int64 -> I64
lit64 n = RAW (A (AInt 64 (fromIntegral n)))

add64 :: I64 -> I64 -> I64
add64 x y = RAW (call "bvadd" [smtlib x, smtlib y])

noSignedOverflowAdd64 :: I64 -> I64 -> SBool
noSignedOverflowAdd64 x y =
  not_ (RAW (eqRaw (signBit x) (signBit y)))
    .|| RAW (eqRaw (signBit x) (signBit (add64 x y)))

signedAdd64Spec :: I64 -> I64 -> I64 -> SBool
signedAdd64Spec x y result =
  noSignedOverflowAdd64 x y `implies` (result === add64 x y)

(===) :: Typeable t => Expr t -> Expr t -> SBool
lhs === rhs = RAW (call "=" [smtlib lhs, smtlib rhs])

(.&&) :: SBool -> SBool -> SBool
lhs .&& rhs = RAW (call "and" [smtlib lhs, smtlib rhs])

(.||) :: SBool -> SBool -> SBool
lhs .|| rhs = RAW (call "or" [smtlib lhs, smtlib rhs])

not_ :: SBool -> SBool
not_ x = RAW (call "not" [smtlib x])

implies :: SBool -> SBool -> SBool
implies lhs rhs = RAW (call "=>" [smtlib lhs, smtlib rhs])

infix 4 ===

infixr 3 .&&

infixr 2 .||

signBit :: I64 -> SExpr Atom
signBit x = call "(_ extract 63 63)" [smtlib x]

eqRaw :: SExpr Atom -> SExpr Atom -> SExpr Atom
eqRaw lhs rhs = call "=" [lhs, rhs]

call :: String -> [SExpr Atom] -> SExpr Atom
call f args = L (A (AAtom f) : args)
