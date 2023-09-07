{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module SMT where

import Data.SCargot
import Data.SCargot.Repr.Basic
import qualified Data.Text as T

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
  | AConj

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
  AConj -> "and"
