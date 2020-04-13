# Whenever

The idea is to build an end-to-end encrypted calendar that supports sharing. Ideally as little information about events is stored on the server as possible. The server's responsibilities will be retrieving calendars for users, retrieving events by ID and attempting to ensure calendar event indexes stay in sync.

## Development

### System requirements

* [Nix](https://nixos.org/nix)
* [Docker](https://docs.docker.com/install/)

### Setup

Enter the Nix shell.

```
$ nix-shell
```

Start the database service.

```
$ docker-compose up -d database
```

Setup the database.

```
$ cabal new-run whenever-database:database -- setup
```

Then run the app.

```
$ cabal new-run whenever-server:whenever
```
