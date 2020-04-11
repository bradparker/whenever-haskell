{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event.Unencrypted
  ( UnencryptedEvent (..),
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data UnencryptedEvent
  = UnencryptedEvent
      { id :: UUID,
        title :: Text,
        startsAt :: UTCTime,
        endsAt :: UTCTime
      }
  deriving (Generic, Show)
