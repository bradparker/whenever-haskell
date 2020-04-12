{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar
  ( module Whenever.Calendar.Encrypted,
    module Whenever.Calendar.EventIndex,
    module Whenever.Calendar.Unencrypted,
    encryptCalendar,
    decryptCalendar,
    newCalendar,
    upsertCalendarEvent,
  )
where

import Control.Lens ((%~), (&), (^.))
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (blockSize, ecbDecrypt, ecbEncrypt)
import Crypto.Data.Padding.Extended (Format (ZERO), pad, unpadZero)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product.Fields (field)
import Data.Time (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (UUID)
import Whenever.Calendar.Encrypted
import Whenever.Calendar.EventIndex
import Whenever.Calendar.Unencrypted

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t epoch)) `div` 1000000

utcTimeToMillis :: UTCTime -> Integer
utcTimeToMillis = (`div` 1000) . utcTimeToMicros

encryptEventIndex :: AES256 -> EventIndex -> ByteString
encryptEventIndex cipher unencryptedEventIndex =
  ecbEncrypt
    cipher
    ( pad
        (ZERO (blockSize cipher))
        (LBS.toStrict (JSON.encode unencryptedEventIndex))
    )

encryptCalendar :: AES256 -> UnencryptedCalendar -> EncryptedCalendar
encryptCalendar cipher unencrypted =
  EncryptedCalendar
    (unencrypted ^. field @"id")
    (encryptEventIndex cipher (unencrypted ^. field @"eventIndex"))

decryptEventIndex :: AES256 -> ByteString -> Either String EventIndex
decryptEventIndex cipher ciphertext =
  let plaintext = ecbDecrypt cipher ciphertext
   in JSON.eitherDecodeStrict (unpadZero plaintext)

decryptCalendar :: AES256 -> EncryptedCalendar -> Either String UnencryptedCalendar
decryptCalendar cipher encrypted =
  UnencryptedCalendar
    (encrypted ^. field @"id")
    <$> decryptEventIndex cipher (encrypted ^. field @"eventIndex")

newCalendar :: UUID -> UnencryptedCalendar
newCalendar calendarId =
  UnencryptedCalendar
    { id = calendarId,
      eventIndex = emptyEventIndex
    }

upsertCalendarEvent ::
  UUID ->
  UTCTime ->
  UTCTime ->
  UnencryptedCalendar ->
  UnencryptedCalendar
upsertCalendarEvent eventId eventStart eventEnd calendar =
  calendar
    & field @"eventIndex"
      %~ updateEventIndex
        eventId
        (fromInteger (utcTimeToMillis eventStart))
        (fromInteger (utcTimeToMillis eventEnd))
