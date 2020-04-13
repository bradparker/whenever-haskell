{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (unless, when)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Network.URI (URI)
import qualified Network.URI as URI
import System.Directory (listDirectory)
import System.Environment (getArgs, getEnv)
import System.Exit (die)
import System.FilePath ((<.>), (</>), takeBaseName, takeExtension)
import System.Process (readProcess)

migrationsTableName :: String
migrationsTableName = "_migrations"

parseDatabaseUrl :: String -> IO URI
parseDatabaseUrl databaseUrl =
  case URI.parseURI databaseUrl of
    Nothing -> die $ "Malformed URL: " ++ databaseUrl
    Just uri -> pure uri

databaseName :: URI -> String
databaseName = drop 1 . URI.uriPath

databaseHost :: URI -> String
databaseHost = maybe "" URI.uriRegName . URI.uriAuthority

databaseUsername :: URI -> String
databaseUsername =
  takeWhile (/= ':')
    . takeWhile (/= '@')
    . maybe "" URI.uriUserInfo
    . URI.uriAuthority

databasePort :: URI -> String
databasePort = drop 1 . maybe "" URI.uriPort . URI.uriAuthority

awaitDatabaseReady :: URI -> IO ()
awaitDatabaseReady uri =
  void $
    readProcess
      "pg_isready"
      ["-h", databaseHost uri, "-p", databasePort uri]
      ""

databaseExists :: URI -> String -> IO Bool
databaseExists uri dbName = do
  databases <-
    lines
      <$> readProcess
        "psql"
        [ "--host",
          databaseHost uri,
          "--port",
          databasePort uri,
          "--username",
          databaseUsername uri,
          "postgres",
          "--tuples-only",
          "--no-align",
          "--command",
          "SELECT datname FROM pg_database;"
        ]
        ""
  pure $ dbName `elem` databases

execCommand :: URI -> String -> IO [String]
execCommand uri command =
  lines
    <$> readProcess
      "psql"
      [ URI.uriToString id uri "",
        "--tuples-only",
        "--no-align",
        "--command",
        command
      ]
      ""

ensureMigrationsTable :: URI -> IO ()
ensureMigrationsTable uri =
  void $ execCommand uri $
    "create table if not exists "
      <> migrationsTableName
      <> " (name text)"

appliedMigrations :: URI -> IO [String]
appliedMigrations uri =
  execCommand uri $ "select name from " <> migrationsTableName

applyMigration :: URI -> FilePath -> IO ()
applyMigration uri filename = do
  let name = takeBaseName filename
  migrations <- appliedMigrations uri
  unless (name `elem` migrations) $ do
    void $ execCommand uri =<< readFile filename
    void $ execCommand uri $
      "insert into " <> migrationsTableName <> " (name) values ('" <> name <> "')"

migrateDatabase :: URI -> FilePath -> IO ()
migrateDatabase uri dir = do
  ensureMigrationsTable uri
  applied <- fmap ((dir </>) . (<.> ".sql")) <$> appliedMigrations uri
  print applied
  files <- fmap (dir </>) . filter ((".sql" ==) . takeExtension) <$> listDirectory dir
  print files
  case listToMaybe (reverse applied) of
    Nothing ->
      traverse_
        (applyMigration uri)
        files
    Just mostRecent ->
      traverse_
        (applyMigration uri)
        (drop 1 (dropWhile (/= mostRecent) files))

createDatabase :: URI -> IO ()
createDatabase uri = do
  awaitDatabaseReady uri
  exists <- databaseExists uri $ databaseName uri
  when exists $ putStrLn $ "Database " ++ databaseName uri ++ " exists. Skipping."
  unless exists $ do
    putStrLn $ "Creating database " ++ databaseName uri
    void $
      readProcess
        "createdb"
        [ "--host",
          databaseHost uri,
          "--port",
          databasePort uri,
          "--username",
          databaseUsername uri,
          databaseName uri
        ]
        ""
    putStrLn $ "Created " ++ databaseName uri

dropDatabase :: URI -> IO ()
dropDatabase uri = do
  awaitDatabaseReady uri
  exists <- databaseExists uri $ databaseName uri
  unless exists $ putStrLn $ "Database " ++ databaseName uri ++ " doesn't exist. Skipping."
  when exists $ do
    putStrLn $ "Dropping database " ++ databaseName uri
    void $
      readProcess
        "dropdb"
        [ "--host",
          databaseHost uri,
          "--port",
          databasePort uri,
          "--username",
          databaseUsername uri,
          databaseName uri
        ]
        ""
    putStrLn $ "Dropped " ++ databaseName uri

setupDatabase :: URI -> FilePath -> IO ()
setupDatabase uri migrationsDir = do
  createDatabase uri
  migrateDatabase uri migrationsDir

resetDatabase :: URI -> FilePath -> IO ()
resetDatabase uri migrationsDir = do
  dropDatabase uri
  setupDatabase uri migrationsDir

data Command
  = Create
  | Drop
  | Migrate
  | Setup
  | Reset

parseCommand :: String -> Maybe Command
parseCommand "create" = Just Create
parseCommand "drop" = Just Drop
parseCommand "migrate" = Just Migrate
parseCommand "setup" = Just Setup
parseCommand "reset" = Just Reset
parseCommand _ = Nothing

main :: IO ()
main = do
  uri <- parseDatabaseUrl =<< getEnv "DATABASE_URL"
  command <- (parseCommand =<<) . listToMaybe <$> getArgs
  let migrationsDir = "./database/migrations"
  case command of
    Just Create -> createDatabase uri
    Just Drop -> dropDatabase uri
    Just Migrate -> migrateDatabase uri migrationsDir
    Just Setup -> setupDatabase uri migrationsDir
    Just Reset -> resetDatabase uri migrationsDir
    Nothing -> die "ERROR: Unknown command"
