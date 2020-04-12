{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar.Encrypted
  ( EncryptedCalendar (..),
  )
where

import Data.ByteString (ByteString)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EncryptedCalendar
  = EncryptedCalendar
      { id :: UUID,
        eventIndex :: ByteString
      }
  deriving (Generic, Show)
