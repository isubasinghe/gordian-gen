{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.MaybeIntSpec where

import qualified Data.Text as T
import AutoDerive.SMTDatatype
import EAST
import EDSL.Bool
import EDSL.Elt
import EDSL.Exp
import EDSL.Match
import EDSL.Maybe

-- A small total spec over Maybe Int:
--   Nothing  -> result must be 0
--   Just val -> result must be val
maybeIntPost :: Exp (Maybe Int) -> Exp Int -> Exp Bool
maybeIntPost input result =
  match
    ( \case
        Nothing_ -> result `Eq` Const 0
        Just_ value -> result `Eq` value
    )
    input

maybeInput :: Exp (Maybe Int)
maybeInput = Var (Idx "input")

maybeResult :: Exp Int
maybeResult = Var (Idx "result")

maybeIntPostBitVecSMT :: T.Text
maybeIntPostBitVecSMT = printSMT (translate (maybeIntPost maybeInput maybeResult))

maybeIntPostDatatypeDecl :: T.Text
maybeIntPostDatatypeDecl = printADTDecl maybeInput

maybeIntPostDatatypeSMT :: T.Text
maybeIntPostDatatypeSMT = printSMTDatatype (translateDatatype (maybeIntPost maybeInput maybeResult))
