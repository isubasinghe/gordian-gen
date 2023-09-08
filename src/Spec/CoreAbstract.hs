{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Spec.CoreAbstract where

import EAST

pre :: Expr (TMaybe TBV32) -> Expr TBV32
pre maybe32 = caseMaybeTBV32 maybe32 (\x -> x `EADD` EBV32 1) (EBV32 0)

constMaybe :: Expr (TMaybe TBV32)
constMaybe = VAR "hello"

pre' :: Expr TBV32
pre' = pre constMaybe

asd :: Expr (TTuple TBV32 TBV64)
asd = VAR "asd"

funASD :: Expr (TTuple TBV32 TBV64)
funASD = FUN0 "nothingness" asd

fun1ASD :: Expr (TTuple TBV32 TBV64)
fun1ASD = FUN1 "nextfun" constMaybe asd

fstASD :: Expr TBV32
fstASD = FST asd

sndASD = SND asd

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

protectedPre :: Expr TCh -> Expr TMsgInfo -> Expr (TMaybe (TTuple TCh TMsgInfo)) -> Expr TBool
protectedPre ch mi lc_unhandled_ppcall = CONJ [checkPPCall]
  where
    checkPPCall :: Expr TBool
    checkPPCall = lc_unhandled_ppcall `EQU` EMAYBE_JUSTC (MKTUP ch mi)

chVAR :: Expr TCh
chVAR = VAR "ch"

miVAR :: Expr TMsgInfo
miVAR = VAR "mi"

ppcallVAR :: Expr (TMaybe (TTuple TCh TMsgInfo))
ppcallVAR = VAR "lc_unhandled_ppcall"

setChVAR :: Expr (TSet TCh)
setChVAR = VAR "chs"

runningPDVar :: Expr TPD
runningPDVar = VAR "lc_running_pd"

-- receiveOracle :: Expr
unhandledNotifiedVar :: Expr (TSet TCh)
unhandledNotifiedVar = VAR "lc_unhandled_notified"

unhandledReplVar :: Expr (TMaybe TMsgInfo)
unhandledReplVar = VAR "lc_unhandled_reply"

lastHandledNotifiedVar :: Expr (TSet TCh)
lastHandledNotifiedVar = VAR "lc_last_handled_notified"

protectedPreFn :: Expr TBool
protectedPreFn = FUN3 "protected-pre" chVAR miVAR ppcallVAR (protectedPre chVAR miVAR ppcallVAR)

protectedPost :: Expr (TMaybe (TTuple TCh TMsgInfo))
protectedPost = undefined

notifiedPre :: Expr TCh -> Expr (TSet TCh) -> Expr TBool
notifiedPre = ELEM

notifiedPreFn :: Expr TBool
notifiedPreFn = notifiedPre chVAR setChVAR

notifiedPost :: Expr (TTuple (TSet TCh) (TSet TCh))
notifiedPost = undefined
