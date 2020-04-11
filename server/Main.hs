{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Data.Time
  ( ParseTime,
    defaultTimeLocale,
    iso8601DateFormat,
    parseTimeM,
  )
import qualified Data.UUID.V4 as UUID
import Whenever (EncryptedEvent (..), UnencryptedEvent (..))

iso8601ParseM :: (Monad m, ParseTime t) => String -> m t
iso8601ParseM =
  parseTimeM
    False
    defaultTimeLocale
    (iso8601DateFormat (Just "%H:%M:%SZ"))

main :: IO ()
main = do
  uuid <- UUID.nextRandom
  print
    EncryptedEvent
      { id = uuid,
        title = "This'll be cipher-text",
        startsAt = 1000,
        endsAt = 1001
      }
  start <- iso8601ParseM "2020-01-01T00:00:00Z"
  end <- iso8601ParseM "2020-01-01T01:00:00Z"
  print
    UnencryptedEvent
      { id = uuid,
        title = "This'll be plain-text",
        startsAt = start,
        endsAt = end
      }
