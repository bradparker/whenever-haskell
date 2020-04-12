{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar.EventIndex
  ( EventIndex (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.IntMap (IntMap)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EventIndex
  = EventIndex
      { startsAt :: IntMap UUID,
        endsAt :: IntMap UUID
      }
  deriving (Generic, Show)

instance FromJSON EventIndex

instance ToJSON EventIndex
