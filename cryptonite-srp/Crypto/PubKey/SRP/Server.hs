{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Crypto.PubKey.SRP.Server
  ( calculatePublic,
    calculateShared,
    module Crypto.PubKey.SRP.Shared
  )
where

import Crypto.PubKey.SRP.Shared
import Crypto.Hash ( HashAlgorithm)
import Crypto.Number.ModArithmetic (expSafe)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Data.ByteArray (ScrubbedBytes, convert)

calculatePublic ::
  forall alg.
  HashAlgorithm alg =>
  Params alg ->
  Verifier ->
  PrivateNumber ->
  PublicNumber
calculatePublic params@(Params p g _ _) verifier (PrivateNumber b) =
  PublicNumber $ multiplier params * os2ip verifier + expSafe g b p

calculateShared ::
  forall alg.
  HashAlgorithm alg =>
  Params alg ->
  Verifier ->
  PrivateNumber ->
  PublicNumber ->
  PublicNumber ->
  SharedKey
calculateShared
  (Params p _ bits alg)
  (Verifier verifier)
  (PrivateNumber serverPriv)
  (PublicNumber serverPub)
  (PublicNumber clientPub) =
    let deserialize = i2ospOf_ @ScrubbedBytes bits
        h = hashManyWith alg
        u = os2ip $ h [deserialize clientPub, deserialize serverPub]
        s = expSafe (clientPub * expSafe (os2ip verifier) u p) serverPriv p
     in SharedKey $ convert $ h [deserialize s]
