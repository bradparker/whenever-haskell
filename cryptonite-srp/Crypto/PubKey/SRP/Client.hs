{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Crypto.PubKey.SRP.Client
  ( calculatePublic,
    calculateShared,
    module Crypto.PubKey.SRP.Shared,
  )
where

import Crypto.Hash (HashAlgorithm)
import Crypto.Number.ModArithmetic (expSafe)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.PubKey.SRP.Shared
import Data.ByteArray (ByteArrayAccess, ScrubbedBytes, convert)

newtype Password = Password ScrubbedBytes
  deriving (Show, Eq, ByteArrayAccess)

calculatePublic :: forall alg. Params alg -> PrivateNumber -> PublicNumber
calculatePublic (Params n g _ _) (PrivateNumber a) =
  PublicNumber $ expSafe g a n

calculateShared ::
  forall alg.
  HashAlgorithm alg =>
  Params alg ->
  Salt ->
  Password ->
  PrivateNumber ->
  PublicNumber ->
  PublicNumber ->
  SharedKey
calculateShared
  params@(Params p g bits alg)
  (Salt salt)
  (Password pass)
  (PrivateNumber clientPriv)
  (PublicNumber clientPub)
  (PublicNumber hostPub) =
    let deserialize = i2ospOf_ @ScrubbedBytes bits
        h = hashManyWith alg
        x = os2ip $ h [convert salt, pass]
        u = os2ip $ h [deserialize clientPub, deserialize hostPub]
        s = expSafe (hostPub - multiplier params * expSafe g x p) (clientPriv + u * x) p
     in SharedKey $ convert $ h [deserialize s]
