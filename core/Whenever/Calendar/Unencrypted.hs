{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar.Unencrypted
  ( UnencryptedCalendar (..),
  )
where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Whenever.Calendar.EventIndex (EventIndex)

data UnencryptedCalendar
  = UnencryptedCalendar
      { id :: UUID,
        eventIndex :: EventIndex
      }
  deriving (Generic, Show)
