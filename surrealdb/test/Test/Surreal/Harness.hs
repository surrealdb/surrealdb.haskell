{-# LANGUAGE OverloadedStrings #-}

-- | A process spawn test harness. It locates a @surreal@ binary, starts an in
-- memory server on a free port, polls the health endpoint until it is ready,
-- and tears the process down afterwards. Modelled on the JavaScript SDK's
-- integration test helpers.
module Test.Surreal.Harness
  ( Server (..)
  , findSurreal
  , withServer
  , startServer
  , stopServer
  , wsUrl
  , httpUrl
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, bracket, try)
import           Control.Monad            (void)
import qualified Data.ByteString.Lazy     as BL
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Client
import           Network.Socket
import           System.Directory         (findExecutable)
import           System.Environment       (lookupEnv)
import           System.Process

-- | A running SurrealDB server.
data Server = Server
  { srvPort   :: !Int
  , srvHandle :: !ProcessHandle
  }

-- | Locate the @surreal@ binary from the @SURREAL_BIN@ environment variable or
-- the @PATH@.
findSurreal :: IO (Maybe FilePath)
findSurreal = do
  fromEnv <- lookupEnv "SURREAL_BIN"
  case fromEnv of
    Just p | not (null p) -> pure (Just p)
    _                     -> findExecutable "surreal"

-- | The WebSocket URL for a server.
wsUrl :: Server -> Text
wsUrl s = "ws://127.0.0.1:" <> T.pack (show (srvPort s)) <> "/rpc"

-- | The HTTP URL for a server.
httpUrl :: Server -> Text
httpUrl s = "http://127.0.0.1:" <> T.pack (show (srvPort s)) <> "/rpc"

-- | Start a server, run an action, then stop it. Skips the action by returning
-- 'Nothing' if no binary is available.
withServer :: FilePath -> (Server -> IO a) -> IO a
withServer bin = bracket (startServer bin) stopServer

startServer :: FilePath -> IO Server
startServer bin = do
  port <- getFreePort
  let args =
        [ "start"
        , "--user", "root"
        , "--pass", "root"
        , "--bind", "127.0.0.1:" ++ show port
        , "memory"
        ]
  (_, _, _, ph) <- createProcess (proc bin args)
    { std_out = NoStream, std_err = NoStream }
  waitReady port 100
  pure (Server port ph)

stopServer :: Server -> IO ()
stopServer s = do
  terminateProcess (srvHandle s)
  void (waitForProcess (srvHandle s))

-- Poll the health endpoint until it responds or the attempts run out.
waitReady :: Int -> Int -> IO ()
waitReady _    0       = error "surreal server did not become ready in time"
waitReady port attempts = do
  manager <- newManager defaultManagerSettings
  ok <- probe manager
  if ok then pure () else threadDelay 100000 >> waitReady port (attempts - 1)
  where
    probe manager = do
      req  <- parseRequest ("http://127.0.0.1:" ++ show port ++ "/health")
      resp <- try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString))
      pure (either (const False) (const True) resp)

getFreePort :: IO Int
getFreePort =
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    port <- socketPort sock
    pure (fromIntegral port)
