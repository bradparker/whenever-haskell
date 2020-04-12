# Whenever

The idea is to build an end-to-end encrypted calendar that supports sharing. Ideally as little information about events is stored on the server as possible. The server's responsibilities will be retrieving calendars for users, retrieving events by ID and attempting to ensure calendar event indexes stay in sync.

## Development

Have [Nix](https://nixos.org/nix) installed and enter the shell.

```
$ nix-shell
```

Then run the app.

```
$ cabal new-run whenever-server:whenever
```
