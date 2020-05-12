{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import qualified Crypto.PubKey.SRP.Client as Client
import qualified Crypto.PubKey.SRP.Server as Server
import Data.ByteArray (convert, convert)
import Hedgehog ((===), Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

genIdentifier :: Gen Client.Identifier
genIdentifier =
  Client.Identifier . convert <$> Gen.utf8 (Range.linear 0 100) Gen.unicodeAll

genPassword :: Gen Client.Password
genPassword =
  Client.Password . convert <$> Gen.utf8 (Range.linear 0 100) Gen.unicodeAll

genSalt :: Gen Client.Salt
genSalt =
  Client.Salt . convert <$> Gen.utf8 (Range.linear 0 100) Gen.unicodeAll

prop_the_protocol :: Property
prop_the_protocol =
  property $ do
    let params = Client.params4096
    identifier <- forAll genIdentifier
    password <- forAll genPassword
    salt <- forAll genSalt
    clientPriv <- Client.PrivateNumber <$> forAll (Gen.integral (Range.linear 1 (Client.params_p params)))
    serverPriv <- Server.PrivateNumber <$> forAll (Gen.integral (Range.linear 1 (Server.params_p params)))
    let verifier = Client.calculateVerifier params salt password
        clientPub = Client.calculatePublic params clientPriv
        serverPub = Server.calculatePublic params verifier serverPriv
        clientKey = Client.calculateShared params salt password clientPriv clientPub serverPub
        serverKey = Server.calculateShared params verifier serverPriv serverPub clientPub
        clientMatcher = Client.calculateMatcher params identifier salt clientPub serverPub clientKey
        serverMatcher = Server.calculateMatcher params identifier salt clientPub serverPub serverKey
        serverProof = Server.calculateProof params clientPub serverKey serverMatcher
        clientProof = Client.calculateProof params clientPub clientKey clientMatcher
    serverMatcher === clientMatcher
    serverProof === clientProof

main :: IO ()
main =
  defaultMain $
    testGroup
      "cryptonite-srp"
      [ testProperty
          "the protocol"
          prop_the_protocol
      ]
