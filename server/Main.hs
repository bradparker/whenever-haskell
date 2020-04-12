{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (eitherCryptoError)
import Data.ByteString (ByteString)
import qualified Data.IntMap as IntMap
import Data.Time
  ( ParseTime,
    UTCTime,
    defaultTimeLocale,
    diffTimeToPicoseconds,
    diffUTCTime,
    iso8601DateFormat,
    parseTimeM,
  )
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.UUID.V4 as UUID
import Whenever
  ( EventIndex (..),
    UnencryptedCalendar (..),
    UnencryptedEvent (..),
    decryptCalendar,
    decryptEvent,
    encryptCalendar,
    encryptEvent,
  )

iso8601ParseM :: (Monad m, ParseTime t) => String -> m t
iso8601ParseM =
  parseTimeM
    False
    defaultTimeLocale
    (iso8601DateFormat (Just "%H:%M:%SZ"))

key :: ByteString
key = "yUiUIIs1bM8zleXFQKNk6mtGODbFU3Eu"

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros t =
  diffTimeToPicoseconds (realToFrac (diffUTCTime t epoch)) `div` 1000000

utcTimeToMillis :: UTCTime -> Integer
utcTimeToMillis = (`div` 1000) . utcTimeToMicros

main :: IO ()
main = do
  eventId <- UUID.nextRandom
  start <- iso8601ParseM "2020-01-01T00:00:00Z"
  end <- iso8601ParseM "2020-01-01T01:00:00Z"
  let unencryptedEvent =
        UnencryptedEvent
          { id = eventId,
            title = "A plain text title!",
            startsAt = start,
            endsAt = end
          }
  print unencryptedEvent
  calendarId <- UUID.nextRandom
  let unencryptedCalendar =
        UnencryptedCalendar
          { id = calendarId,
            eventIndex =
              EventIndex
                { startsAt = IntMap.singleton (fromInteger (utcTimeToMillis start)) eventId,
                  endsAt = IntMap.singleton (fromInteger (utcTimeToMillis end)) eventId
                }
          }
  print unencryptedCalendar
  case eitherCryptoError (cipherInit @AES256 key) of
    Left e -> print e
    Right cipher -> do
      let encryptedEvent = encryptEvent cipher unencryptedEvent
      print encryptedEvent
      print $ decryptEvent cipher encryptedEvent
      let encryptedCalendar = encryptCalendar cipher unencryptedCalendar
      print encryptedCalendar
      print $ decryptCalendar cipher encryptedCalendar
