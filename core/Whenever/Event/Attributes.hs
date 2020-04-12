{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Event.Attributes
  ( EventAttributes (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data EventAttributes
  = EventAttributes
      { title :: Text,
        startsAt :: UTCTime,
        endsAt :: UTCTime
      }
  deriving (Generic, Show)

instance FromJSON EventAttributes

instance ToJSON EventAttributes
