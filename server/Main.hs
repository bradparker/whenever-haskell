{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Control.Monad.Except (runExceptT)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit)
import Crypto.Error (eitherCryptoError)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor (void)
import qualified Data.Set as Set
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.UUID.V4 as UUID
import System.Environment (getEnv)
import Whenever
  ( EventAttributes (..),
    calendarEventsBetween,
    decryptCalendar,
    decryptEvent,
    encryptCalendar,
    encryptEvent,
    event,
    newCalendar,
  )
import Whenever.Repository (loadCalendars, loadEvents, saveCalendar, saveEvent)
import Whenever.Repository.Postgres (withRepository)

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
      low <- iso8601ParseM "2019-01-01T00:00:00Z"
      high <- iso8601ParseM "2021-01-01T01:00:00Z"
      print $ calendarEventsBetween low high unencryptedCalendar
      databaseUrl <- BS.pack <$> getEnv "DATABASE_URL"
      withRepository (1, 5, databaseUrl) $ \repository -> do
        result <- runExceptT $ do
          void $ saveCalendar repository encryptedCalendar
          void $ saveEvent repository (encryptedEvent, encryptedCalendar)
          calendars <- loadCalendars repository (Set.singleton calendarId)
          events <- loadEvents repository (Set.singleton eventId)
          pure (calendars, events)
        print result
