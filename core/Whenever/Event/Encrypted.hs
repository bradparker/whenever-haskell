{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event.Encrypted
  ( EncryptedEvent (..),
  )
where

import Data.ByteString (ByteString)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EncryptedEvent
  = EncryptedEvent
      { id :: UUID,
        content :: ByteString
      }
  deriving (Generic, Show)
