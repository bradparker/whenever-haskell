{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Repository
  ( Repository (..),
    EventsRepository (..),
    CalendarsRepository (..),
    saveEvent,
    loadEvents,
    saveCalendar,
    loadCalendars,
  )
where

import Control.Lens ((^.))
import Data.Generics.Product.Fields (field)
import Data.Set (Set)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Whenever (EncryptedCalendar, EncryptedEvent)

data EventsRepository m
  = EventsRepository
      { save :: EncryptedEvent -> m (),
        load :: Set UUID -> m [EncryptedEvent]
      }
  deriving (Generic)

data CalendarsRepository m
  = CalendarsRepository
      { save :: EncryptedCalendar -> m (),
        load :: Set UUID -> m [EncryptedCalendar]
      }
  deriving (Generic)

data Repository m
  = Repository
      { events :: EventsRepository m,
        calendars :: CalendarsRepository m
      }
  deriving (Generic)

saveEvent :: Applicative m => Repository m -> (EncryptedEvent, EncryptedCalendar) -> m ()
saveEvent repository (event, calendar) =
  saveCalendar repository calendar
    *> (repository ^. field @"events" . field @"save") event

loadEvents :: Repository m -> Set UUID -> m [EncryptedEvent]
loadEvents repository = repository ^. field @"events" . field @"load"

saveCalendar :: Repository m -> EncryptedCalendar -> m ()
saveCalendar repository = repository ^. field @"calendars" . field @"save"

loadCalendars :: Repository m -> Set UUID -> m [EncryptedCalendar]
loadCalendars repository = repository ^. field @"calendars" . field @"load"

