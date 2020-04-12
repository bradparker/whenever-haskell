{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Calendar.EventIndex
  ( EventIndex (..),
    emptyEventIndex,
    updateEventIndex,
    eventsBetween
  )
where

import Control.Lens ((%~), (&), (^.), _1, _3)
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

elemsBetween :: Int -> Int -> IntMap a -> [a]
elemsBetween low high imap =
   IntMap.elems $ IntMap.splitLookup high (IntMap.splitLookup low imap ^. _3) ^. _1

eventsStartingBetween ::
  Int ->
  Int ->
  EventIndex ->
  Set UUID
eventsStartingBetween low high index =
  mconcat $ elemsBetween low high (index ^. field @"startsAt")

eventsEndingBetween ::
  Int ->
  Int ->
  EventIndex ->
  Set UUID
eventsEndingBetween low high index =
  mconcat $ elemsBetween low high (index ^. field @"endsAt")

eventsBetween ::
  Int ->
  Int ->
  EventIndex ->
  Set UUID
eventsBetween low high index =
  Set.intersection
    (eventsStartingBetween low high index)
    (eventsEndingBetween low high index)
