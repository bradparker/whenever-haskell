{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event.Unencrypted
  ( UnencryptedEvent (..),
  )
where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Whenever.Event.Attributes (EventAttributes)

data UnencryptedEvent
  = UnencryptedEvent
      { id :: UUID,
        attributes :: EventAttributes
      }
  deriving (Generic, Show)
