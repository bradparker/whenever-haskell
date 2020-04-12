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
import Data.Time
  ( ParseTime,
    defaultTimeLocale,
    iso8601DateFormat,
    parseTimeM,
  )
import qualified Data.UUID.V4 as UUID
import Whenever
  ( EventAttributes (..),
    decryptCalendar,
    decryptEvent,
    encryptCalendar,
    encryptEvent,
    event,
    newCalendar,
  )

iso8601ParseM :: (Monad m, ParseTime t) => String -> m t
iso8601ParseM =
  parseTimeM
    False
    defaultTimeLocale
    (iso8601DateFormat (Just "%H:%M:%SZ"))

key :: ByteString
key = "yUiUIIs1bM8zleXFQKNk6mtGODbFU3Eu"

main :: IO ()
main = do
  calendarId <- UUID.nextRandom
  eventId <- UUID.nextRandom
  start <- iso8601ParseM "2020-01-01T00:00:00Z"
  end <- iso8601ParseM "2020-01-01T01:00:00Z"
  let (unencryptedEvent, unencryptedCalendar) =
        event
          eventId
          ( EventAttributes
              { title = "A plain text title!",
                startsAt = start,
                endsAt = end
              }
          )
          (newCalendar calendarId)
  print unencryptedCalendar
  print unencryptedEvent
  case eitherCryptoError (cipherInit @AES256 key) of
    Left e -> print e
    Right cipher -> do
      let encryptedEvent = encryptEvent cipher unencryptedEvent
      print encryptedEvent
      print $ decryptEvent cipher encryptedEvent
      let encryptedCalendar = encryptCalendar cipher unencryptedCalendar
      print encryptedCalendar
      print $ decryptCalendar cipher encryptedCalendar
