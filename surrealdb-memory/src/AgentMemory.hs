-- | The single import for the Agent Memory client. It re-exports the client
-- handle, the memory operations, the chat loop, the status endpoints, the
-- resource namespaces, the request types and the error type.
--
-- A minimal example:
--
-- > import AgentMemory
-- >
-- > main :: IO ()
-- > main = do
-- >   client <- newAgentMemory (defaultAgentMemoryOptions "acme-prod" "sk_..." "https://api.spectron.surrealdb.com")
-- >   _ <- remember client "Alice moved to Berlin" defaultRememberOptions
-- >   answer <- recall client "Where does Alice live?" defaultRecallOptions
-- >   print answer
module AgentMemory
  ( module AgentMemory.Types
  , module AgentMemory.Error
  , module AgentMemory.Client
  , module AgentMemory.Memory
  , module AgentMemory.Chat
  , module AgentMemory.Status
  , module AgentMemory.Namespaces
  ) where

import           AgentMemory.Chat
import           AgentMemory.Client
import           AgentMemory.Error
import           AgentMemory.Memory
import           AgentMemory.Namespaces
import           AgentMemory.Status
import           AgentMemory.Types
