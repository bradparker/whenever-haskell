{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event
  ( module Whenever.Event.Attributes,
    module Whenever.Event.Encrypted,
    module Whenever.Event.Unencrypted,
    encryptEvent,
    decryptEvent,
    event,
  )
where

import Control.Lens ((^.))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (blockSize, ecbDecrypt, ecbEncrypt)
import Crypto.Data.Padding.Extended (Format (ZERO), pad, unpadZero)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product.Fields (field)
import Data.UUID (UUID)
import Whenever.Calendar
import Whenever.Event.Attributes
import Whenever.Event.Encrypted
import Whenever.Event.Unencrypted

encryptAttributes :: AES256 -> EventAttributes -> ByteString
encryptAttributes cipher =
  ecbEncrypt cipher . pad (ZERO (blockSize cipher)) . LBS.toStrict . JSON.encode

encryptEvent :: AES256 -> UnencryptedEvent -> EncryptedEvent
encryptEvent cipher unencrypted =
  EncryptedEvent
    (unencrypted ^. field @"id")
    (encryptAttributes cipher (unencrypted ^. field @"attributes"))

decryptAttributes :: AES256 -> ByteString -> Either String EventAttributes
decryptAttributes cipher =
  JSON.eitherDecodeStrict . unpadZero . ecbDecrypt cipher

decryptEvent :: AES256 -> EncryptedEvent -> Either String UnencryptedEvent
decryptEvent cipher encrypted =
  UnencryptedEvent
    (encrypted ^. field @"id")
    <$> decryptAttributes cipher (encrypted ^. field @"attributes")

event ::
  UUID ->
  EventAttributes ->
  UnencryptedCalendar ->
  (UnencryptedEvent, UnencryptedCalendar)
event eventId eventAttributes calendar =
  ( UnencryptedEvent
      { id = eventId,
        attributes = eventAttributes
      },
    upsertCalendarEvent
      eventId
      (eventAttributes ^. field @"startsAt")
      (eventAttributes ^. field @"endsAt")
      calendar
  )
