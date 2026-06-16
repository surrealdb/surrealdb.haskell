-- | The single import for the Spectron client. It re-exports the client
-- handle, the memory operations, the chat loop, the status endpoints, the
-- resource namespaces, the request types and the error type.
--
-- A minimal example:
--
-- > import Spectron
-- >
-- > main :: IO ()
-- > main = do
-- >   client <- newSpectron (defaultSpectronOptions "acme-prod" "sk_..." "https://api.spectron.surrealdb.com")
-- >   _ <- remember client "Alice moved to Berlin" defaultRememberOptions
-- >   answer <- recall client "Where does Alice live?" defaultRecallOptions
-- >   print answer
module Spectron
  ( module Spectron.Types
  , module Spectron.Error
  , module Spectron.Client
  , module Spectron.Memory
  , module Spectron.Chat
  , module Spectron.Status
  , module Spectron.Namespaces
  ) where

import           Spectron.Chat
import           Spectron.Client
import           Spectron.Error
import           Spectron.Memory
import           Spectron.Namespaces
import           Spectron.Status
import           Spectron.Types
