{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AutoDerive.SMTDatatype
  ( SMTDatatypeRepr (..),
    SMTADT (..),
    printADTDecl,
    printSMTDatatype,
    translateDatatype,
  )
where

import qualified Data.Text as T
import EDSL.Bool ()
import EDSL.Elt
import EDSL.Exp
import EDSL.Maybe ()
import EDSL.Trace
import EDSL.Type

class Elt a => SMTDatatypeRepr a where
  smtDatatypeSort :: proxy a -> T.Text
  smtDatatypeDecls :: proxy a -> [T.Text]
  smtDatatypeExpr :: Exp a -> SMTADT a

newtype SMTADT a = SMTADT {unSMTADT :: T.Text}

printADTDecl :: SMTDatatypeRepr a => proxy a -> T.Text
printADTDecl = T.unlines . smtDatatypeDecls

printSMTDatatype :: SMTADT a -> T.Text
printSMTDatatype = unSMTADT

translateDatatype :: SMTDatatypeRepr a => Exp a -> SMTADT a
translateDatatype = smtDatatypeExpr

instance SMTDatatypeRepr Int where
  smtDatatypeSort _ = "Int"
  smtDatatypeDecls _ = []
  smtDatatypeExpr (Const n) = SMTADT (T.pack (show n))
  smtDatatypeExpr (Var (Idx v)) = SMTADT v
  smtDatatypeExpr (Match _ e) = smtDatatypeExpr e
  smtDatatypeExpr (Case x xs) = translateADTCase x xs
  smtDatatypeExpr (Prj ix x) = projectADT ix x
  smtDatatypeExpr _ = error "translateDatatype: unsupported Int expression"

instance SMTDatatypeRepr Bool where
  smtDatatypeSort _ = "Bool"
  smtDatatypeDecls _ = []
  smtDatatypeExpr (Const b) =
    SMTADT $
      if toElt b
        then "true"
        else "false"
  smtDatatypeExpr (Var (Idx v)) = SMTADT v
  smtDatatypeExpr (Match _ e) = smtDatatypeExpr e
  smtDatatypeExpr (Eq x y) =
    SMTADT $
      "(= "
        <> adtExpr x
        <> " "
        <> adtExpr y
        <> ")"
  smtDatatypeExpr (Case x xs) = translateADTCase x xs
  smtDatatypeExpr (Prj ix x) = projectADT ix x
  smtDatatypeExpr _ = error "translateDatatype: unsupported Bool expression"

instance SMTDatatypeRepr a => SMTDatatypeRepr (Maybe a) where
  smtDatatypeSort _ = "Maybe_" <> smtDatatypeSort (ProxyA @a)
  smtDatatypeDecls _ =
    smtDatatypeDecls (ProxyA @a)
      ++ [ "(declare-datatypes ((Maybe_"
             <> smtDatatypeSort (ProxyA @a)
             <> " 0)) (((Nothing_"
             <> smtDatatypeSort (ProxyA @a)
             <> ") (Just_"
             <> smtDatatypeSort (ProxyA @a)
             <> " (fromJust_"
             <> smtDatatypeSort (ProxyA @a)
             <> " "
             <> smtDatatypeSort (ProxyA @a)
             <> ")))))"
         ]
  smtDatatypeExpr (Var (Idx v)) = SMTADT v
  smtDatatypeExpr (Match _ e) = smtDatatypeExpr e
  smtDatatypeExpr (Tuple _) = error "translateDatatype: constructing Maybe values is not supported yet"
  smtDatatypeExpr (Case x xs) = translateADTCase x xs
  smtDatatypeExpr _ = error "translateDatatype: unsupported Maybe expression"

data ProxyA a = ProxyA

translateADTCase :: (Elt a, SMTDatatypeRepr b) => Exp a -> [(TraceR (EltR a), Exp b)] -> SMTADT b
translateADTCase _ [] = error "translateDatatype: empty case"
translateADTCase _ [(_, r)] = translateDatatype r
translateADTCase x ((tr, r) : rest) =
  SMTADT $
    "(ite "
      <> adtTraceGuard tr x
      <> " "
      <> printSMTDatatype (translateDatatype r)
      <> " "
      <> printSMTDatatype (translateADTCase x rest)
      <> ")"

adtTraceGuard :: forall a. Elt a => TraceR (EltR a) -> Exp a -> T.Text
adtTraceGuard (TraceRtag tag _) x =
  case tag of
    0 -> "((_ is " <> nothingCtor (eltR @a) <> ") " <> adtScrutinee x <> ")"
    1 -> "((_ is " <> justCtor (eltR @a) <> ") " <> adtScrutinee x <> ")"
    _ -> error "translateDatatype: unsupported constructor tag"
adtTraceGuard _ _ = "true"

adtScrutinee :: Exp a -> T.Text
adtScrutinee (Var (Idx v)) = v
adtScrutinee (Match _ e) = adtScrutinee e
adtScrutinee _ = error "translateDatatype: unsupported case scrutinee"

projectADT :: forall s e t. (SMTDatatypeRepr e, Elt t) => TupleIdx s e -> Exp t -> SMTADT e
projectADT PrjZ _ = error "translateDatatype: unsupported direct projection"
projectADT (PrjL ix) e = projectADT ix e
projectADT (PrjR PrjZ) e =
  SMTADT $
    "("
      <> fromJustSelector (eltR @t)
      <> " "
      <> adtScrutinee e
      <> ")"
projectADT (PrjR ix) e = projectADT ix e

adtExpr :: forall a. Elt a => Exp a -> T.Text
adtExpr (Const c) = adtConst (eltR @a) c
adtExpr (Var (Idx v)) = v
adtExpr (Match _ e) = adtExpr e
adtExpr (Prj ix e) = projectADTText ix e
adtExpr _ = error "translateDatatype: unsupported expression"

projectADTText :: forall s e t. Elt t => TupleIdx s e -> Exp t -> T.Text
projectADTText PrjZ _ = error "translateDatatype: unsupported direct projection"
projectADTText (PrjL ix) e = projectADTText ix e
projectADTText (PrjR PrjZ) e =
  "("
    <> fromJustSelector (eltR @t)
    <> " "
    <> adtScrutinee e
    <> ")"
projectADTText (PrjR ix) e = projectADTText ix e

adtConst :: TypeR a -> a -> T.Text
adtConst (TypeRprim (IntegralNumType TypeInt)) n = T.pack (show n)
adtConst (TypeRprim (IntegralNumType TypeInteger)) n = T.pack (show n)
adtConst (TypeRprim (IntegralNumType TypeWord8)) n = T.pack (show n)
adtConst TypeRunit () = "()"
adtConst _ _ = error "translateDatatype: unsupported constant"

nothingCtor :: TypeR r -> T.Text
nothingCtor r = "Nothing_" <> constructorSuffixFromTypeR r

justCtor :: TypeR r -> T.Text
justCtor r = "Just_" <> constructorSuffixFromTypeR r

fromJustSelector :: TypeR r -> T.Text
fromJustSelector r = "fromJust_" <> constructorSuffixFromTypeR r

constructorSuffixFromTypeR :: TypeR r -> T.Text
constructorSuffixFromTypeR (TypeRpair (TypeRprim (IntegralNumType TypeWord8)) rest) = datatypeSortFromTypeR rest
constructorSuffixFromTypeR r = datatypeSortFromTypeR r

datatypeSortFromTypeR :: TypeR r -> T.Text
datatypeSortFromTypeR (TypeRprim (IntegralNumType TypeInt)) = "Int"
datatypeSortFromTypeR (TypeRprim (IntegralNumType TypeInteger)) = "Int"
datatypeSortFromTypeR (TypeRprim (IntegralNumType TypeWord8)) = "Int"
datatypeSortFromTypeR (TypeRprim _) = error "translateDatatype: unsupported primitive datatype sort"
datatypeSortFromTypeR (TypeRpair (TypeRprim (IntegralNumType TypeWord8)) rest) =
  "Maybe_" <> datatypeSortFromTypeR rest
datatypeSortFromTypeR (TypeRpair TypeRunit rest) = datatypeSortFromTypeR rest
datatypeSortFromTypeR (TypeRpair _ _) = error "translateDatatype: unsupported product datatype sort"
datatypeSortFromTypeR TypeRunit = error "translateDatatype: unit has no datatype sort"
datatypeSortFromTypeR (TypeRrec _) = error "translateDatatype: recursive datatype sorts are not supported"
