-- | The single import most applications need. It re-exports the value types,
-- the 'Database.Surreal.Class.SurrealRecord' mapping class, the connection and
-- monad layers, and every RPC method.
--
-- A minimal example:
--
-- > import Database.Surreal
-- > import qualified Data.Map.Strict as Map
-- >
-- > main :: IO ()
-- > main = do
-- >   db <- connect "ws://127.0.0.1:8000/rpc" defaultConnectOpts
-- >   runSurreal db $ do
-- >     _ <- signin (Root "root" "root")
-- >     use (Just "test") (Just "test")
-- >     people <- queryFirst "SELECT * FROM person" Map.empty
-- >     liftIO (print (people :: [Map.Map Text SurrealValue]))
-- >   close db
module Database.Surreal
  ( module Database.Surreal.Types
  , module Database.Surreal.Class
  , module Database.Surreal.Codec
  , module Database.Surreal.Error
  , module Database.Surreal.Engine
  , module Database.Surreal.Connection
  , module Database.Surreal.Monad
  , module Database.Surreal.Auth
  , module Database.Surreal.Api
  , module Database.Surreal.Live
  , module Database.Surreal.Transaction
  ) where

import           Database.Surreal.Api
import           Database.Surreal.Auth
import           Database.Surreal.Class
import           Database.Surreal.Codec
import           Database.Surreal.Connection
import           Database.Surreal.Engine
import           Database.Surreal.Error
import           Database.Surreal.Live
import           Database.Surreal.Monad
import           Database.Surreal.Transaction
import           Database.Surreal.Types
import           Database.Surreal.Value      ()
