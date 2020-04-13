{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Whenever.Repository.Postgres
  ( withRepository,
  )
where

import Control.Exception (bracket)
import Control.Lens ((^.), _1, _2, view)
import Control.Monad.Except (ExceptT (ExceptT))
import Data.Generics.Product.Fields (field')
import Data.Profunctor (dimap, lmap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID (UUID)
import qualified Data.Vector as Vector
import Hasql.Pool (UsageError)
import qualified Hasql.Pool as Pool
import Hasql.Session (statement)
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement, vectorStatement)
import Whenever (EncryptedCalendar (..), EncryptedEvent (..))
import Whenever.Repository
  ( CalendarsRepository (..),
    EventsRepository (..),
    Repository (..),
  )

saveCalendar :: Statement EncryptedCalendar ()
saveCalendar =
  lmap
    encoder
    [resultlessStatement|
      INSERT INTO "calendars" (id, eventIndex)
      VALUES ($1 :: uuid, $2 :: bytea)
      ON CONFLICT (id) DO UPDATE SET eventIndex = $2 :: bytea
    |]
  where
    encoder calendar =
      ( calendar ^. field' @"id",
        calendar ^. field' @"eventIndex"
      )

loadCalendars :: Statement (Set UUID) [EncryptedCalendar]
loadCalendars =
  dimap
    encoder
    decoder
    [vectorStatement|
      SELECT id :: uuid, eventIndex :: bytea
      FROM "calendars"
      WHERE id = ANY($1 :: uuid[])
    |]
  where
    encoder = Vector.fromList . Set.toList
    decoder = fmap (EncryptedCalendar <$> view _1 <*> view _2) . Vector.toList

saveEvent :: Statement EncryptedEvent ()
saveEvent =
  lmap
    encoder
    [resultlessStatement|
      INSERT INTO "events" (id, attributes)
      VALUES ($1 :: uuid, $2 :: bytea)
      ON CONFLICT (id) DO UPDATE SET attributes = $2 :: bytea
    |]
  where
    encoder event =
      ( event ^. field' @"id",
        event ^. field' @"attributes"
      )

loadEvents :: Statement (Set UUID) [EncryptedEvent]
loadEvents =
  dimap
    encoder
    decoder
    [vectorStatement|
      select id :: uuid, attributes :: bytea
      from "events"
      where id = any($1 :: uuid[])
    |]
  where
    encoder = Vector.fromList . Set.toList
    decoder = fmap (EncryptedEvent <$> view _1 <*> view _2) . Vector.toList

withRepository :: Pool.Settings -> (Repository (ExceptT UsageError IO) -> IO a) -> IO a
withRepository settings action =
  bracket
    (Pool.acquire settings)
    Pool.release
    $ \pool ->
      action $
        Repository
          { calendars =
              CalendarsRepository
                { save = \calendar -> ExceptT (Pool.use pool (statement calendar saveCalendar)),
                  load = \calendarIds -> ExceptT (Pool.use pool (statement calendarIds loadCalendars))
                },
            events =
              EventsRepository
                { save = \event -> ExceptT (Pool.use pool (statement event saveEvent)),
                  load = \eventIds -> ExceptT (Pool.use pool (statement eventIds loadEvents))
                }
          }
