BEGIN;

CREATE TABLE IF NOT EXISTS calendars (
  id UUID PRIMARY KEY NOT NULL,
  eventIndex BYTEA NOT NULL
);

COMMIT;
