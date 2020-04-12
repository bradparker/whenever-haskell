{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar.EventIndex
  ( EventIndex (..),
    emptyEventIndex,
    updateEventIndex,
  )
where

import Control.Lens ((%~), (^.), (&))
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Product.Fields (field)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID (UUID)
import GHC.Generics (Generic)

data EventIndex
  = EventIndex
      { id :: Map UUID (Int, Int),
        startsAt :: IntMap (Set UUID),
        endsAt :: IntMap (Set UUID)
      }
  deriving (Generic, Show)

instance FromJSON EventIndex

instance ToJSON EventIndex

emptyEventIndex :: EventIndex
emptyEventIndex = EventIndex mempty mempty mempty

updateEventIndex :: UUID -> Int -> Int -> EventIndex -> EventIndex
updateEventIndex eventId start end index =
  let existing = Map.lookup eventId (index ^. field @"id")
      cleanIndex = case existing of
        Nothing -> index
        Just (oldStart, oldEnd) ->
          index
            & field @"id" %~ Map.delete eventId
            & field @"startsAt" %~ IntMap.adjust (Set.delete eventId) oldStart
            & field @"endsAt" %~ IntMap.adjust (Set.delete eventId) oldEnd
   in cleanIndex
        & field @"id" %~ Map.insert eventId (start, end)
        & field @"startsAt" %~ IntMap.insertWith (<>) start (Set.singleton eventId)
        & field @"endsAt" %~ IntMap.insertWith (<>) start (Set.singleton eventId)
