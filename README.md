# SurrealDB SDK for Haskell

A Haskell SDK for [SurrealDB](https://surrealdb.com), with feature parity to the
official JavaScript SDK. It ships two packages:

- **`surrealdb`** is the database client. It speaks both the WebSocket and HTTP
  transports and both the CBOR (default) and JSON-RPC wire codecs. It covers the
  full method surface: connection and session management, authentication, CRUD,
  graph relations, queries with bound variables, live queries, transactions and
  user defined functions.
- **`surrealdb-spectron`** is a typed client for SurrealDB's Spectron memory and
  knowledge platform. It mirrors the official `@surrealdb/spectron` client:
  `remember`, `recall`, `context`, `reflect`, `forget`, the chat loop and the
  documents, entities, sessions, lifecycle, traces, principals, scopes and keys
  namespaces.

The public API is exposed through a `MonadSurreal` typeclass with a `ReaderT`
based runner, so it composes with any application monad stack.

## Design

Every value sent to or received from SurrealDB passes through a single canonical
`SurrealValue` AST. That AST is the only type with wire instances: one
`Serialise` instance for CBOR and one `ToJSON`/`FromJSON` pair for JSON. All of
SurrealDB's tag and string conventions live behind that one module.

Your own records never touch a codec. They implement `SurrealRecord`, which a
`Generic` instance derives for free, and stay codec blind:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Database.Surreal
import GHC.Generics (Generic)

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance SurrealRecord Person
```

The codec is a value held by the connection, never a type parameter, so the
method API is codec agnostic by construction.

## Installation

There are no package manager releases yet. Add the packages to your project from
this repository.

With cabal, add the repository to your `cabal.project`:

```cabal
packages: .

source-repository-package
  type: git
  location: https://github.com/surrealdb/surrealdb.haskell
  subdir: surrealdb surrealdb-spectron
```

Then add `surrealdb` (and optionally `surrealdb-spectron`) to the
`build-depends` of your component.

With stack, add them under `extra-deps` in `stack.yaml`:

```yaml
extra-deps:
  - git: https://github.com/surrealdb/surrealdb.haskell
    commit: <commit-sha>
    subdirs:
      - surrealdb
      - surrealdb-spectron
```

The project builds with GHC 9.4 and 9.6.

## Usage: SurrealDB

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.Surreal
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Map.Strict as Map

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance SurrealRecord Person

main :: IO ()
main = do
  -- Connect over WebSocket with the default CBOR codec. Use
  -- defaultConnectOpts { coCodec = CodecJson } to switch to JSON-RPC, or an
  -- http:// or https:// URL for the stateless HTTP transport.
  db <- connect "ws://127.0.0.1:8000/rpc" defaultConnectOpts

  runSurreal db $ do
    _ <- signin (Root "root" "root")
    use (Just "test") (Just "test")

    -- Create a record. CRUD methods accept a table name, a record id, or a
    -- record id range as the target.
    _ <- create (onTable "person") (Person "Alice" 30) :: SurrealT IO [Person]

    -- Run a query with bound variables.
    adults <- queryFirst "SELECT * FROM person WHERE age >= $min"
                         (Map.fromList [("min", VInt 18)]) :: SurrealT IO [Person]
    liftIO (print adults)

    -- Select a single record by id.
    one <- select (onRecord (recordIdText "person" "alice")) :: SurrealT IO [Person]
    liftIO (print one)

  close db
```

### Live queries (WebSocket only)

```haskell
runSurreal db $ do
  lq <- live "person"
  -- nextNotification blocks until the next change arrives.
  notification <- nextNotification lq
  liftIO (print (ntfAction notification))
  kill lq
```

### Transactions (WebSocket only)

```haskell
runSurreal db $ withTransaction $ do
  _ <- create (onTable "account") (Person "Bob" 41) :: SurrealT IO [Person]
  _ <- create (onTable "account") (Person "Carol" 29) :: SurrealT IO [Person]
  pure ()
```

`withTransaction` commits on success and cancels if the action throws.

## Usage: Spectron

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Spectron

main :: IO ()
main = do
  client <- newSpectron
    (defaultSpectronOptions "acme-prod" "sk_your_api_key" "https://api.spectron.surrealdb.com")

  -- Store a memory.
  _ <- remember client "Alice moved to Berlin" defaultRememberOptions

  -- Recall relevant memories.
  answer <- recall client "Where does Alice live?" defaultRecallOptions
  print answer

  -- Chat with the memory loop.
  reply <- chat client "What do you know about me?" defaultChatOptions
  print reply
```

Acting on behalf of another principal:

```haskell
let delegated = onBehalfOf client "principal:alice"
_ <- remember delegated "note for Alice" defaultRememberOptions
```

## Errors

The database client throws a single `SurrealError` exception type whose variants
separate the failure modes you need to distinguish: a server error
(`RpcErr`), a transport or connection failure (`TransportErr`, `ConnectErr`),
a codec failure (`CodecErr`), a value that did not fit your Haskell type
(`DecodeErr`), or an operation the transport does not support
(`UnsupportedByTransport`, for example a live query over HTTP).

The Spectron client throws `SpectronError`, classified by HTTP status into kinds
such as `AuthFailed`, `ScopeRejected`, `RateLimited` and `ServerFailed`.

## Development

```sh
cabal build all
cabal test all
```

The test suite has pure unit and property tests that run with no server, and an
integration suite that spawns a real `surreal` binary. The integration tests run
automatically when a `surreal` binary is found on the `PATH` or named by the
`SURREAL_BIN` environment variable, and are skipped otherwise.

## License

Apache License 2.0. See [LICENSE](LICENSE).
