{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.EDSLExample where

import qualified Data.Text as T
import AutoDerive.SMTDatatype
import EAST
import EDSL.Bool
import EDSL.Elt
import EDSL.Exp
import EDSL.Match
import EDSL.Maybe

maybeDefault :: Exp (Maybe Int) -> Exp Int
maybeDefault =
  match $ \case
    Nothing_ -> Const 0
    Just_ n -> n

isNothing :: Exp (Maybe Int) -> Exp Bool
isNothing =
  match $ \case
    Nothing_ -> Const (fromElt True)
    Just_ _ -> Const (fromElt False)

maybeX :: Exp (Maybe Int)
maybeX = Var (Idx "x")

maybeDefaultSMT :: T.Text
maybeDefaultSMT = printSMT (translate (maybeDefault maybeX))

isNothingSMT :: T.Text
isNothingSMT = printSMT (translate (isNothing maybeX))

maybeDatatypeDecl :: T.Text
maybeDatatypeDecl = printADTDecl maybeX

maybeDefaultDatatypeSMT :: T.Text
maybeDefaultDatatypeSMT = printSMTDatatype (translateDatatype (maybeDefault maybeX))

isNothingDatatypeSMT :: T.Text
isNothingDatatypeSMT = printSMTDatatype (translateDatatype (isNothing maybeX))
