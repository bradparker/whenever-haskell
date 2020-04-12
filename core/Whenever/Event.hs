{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event
  ( module Whenever.Event.Encrypted,
    module Whenever.Event.Unencrypted,
    encryptEvent,
    decryptEvent,
  )
where

import Control.Lens ((^.))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (blockSize, ecbDecrypt, ecbEncrypt)
import Crypto.Data.Padding.Extended (Format (ZERO), pad, unpadZero)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Generics.Product.Fields (field', getField)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Whenever.Event.Encrypted
import Whenever.Event.Unencrypted

data Content
  = Content
      { title :: Text,
        startsAt :: UTCTime,
        endsAt :: UTCTime
      }
  deriving (Generic)

instance ToJSON Content

instance FromJSON Content

encryptContent :: AES256 -> Content -> ByteString
encryptContent cipher unencryptedContent =
  ecbEncrypt
    cipher
    ( pad
        (ZERO (blockSize cipher))
        (LBS.toStrict (JSON.encode unencryptedContent))
    )

encryptEvent :: AES256 -> UnencryptedEvent -> EncryptedEvent
encryptEvent cipher unencrypted =
  let unencryptedContent =
        Content
          (unencrypted ^. field' @"title")
          (unencrypted ^. field' @"startsAt")
          (unencrypted ^. field' @"endsAt")
   in EncryptedEvent
        (unencrypted ^. field' @"id")
        (encryptContent cipher unencryptedContent)

decryptContent :: AES256 -> ByteString -> Either String Content
decryptContent cipher ciphertext =
  let plaintext = ecbDecrypt cipher ciphertext
   in JSON.eitherDecodeStrict (unpadZero plaintext)

decryptEvent :: AES256 -> EncryptedEvent -> Either String UnencryptedEvent
decryptEvent cipher encrypted =
  let unencryptedContent = decryptContent cipher (encrypted ^. field' @"content")
   in UnencryptedEvent
        (encrypted ^. field' @"id")
        <$> (unencryptedContent <&> (getField @"title"))
        <*> (unencryptedContent <&> (getField @"startsAt"))
        <*> (unencryptedContent <&> (getField @"endsAt"))
