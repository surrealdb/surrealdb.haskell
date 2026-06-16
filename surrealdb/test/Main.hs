-- | The surrealdb test entry point. The pure codec, value and RPC suites run
-- everywhere. The integration suite runs only when a @surreal@ binary is found,
-- otherwise it is reported as skipped.
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Runners (NumThreads (..))

import qualified Test.Surreal.CodecSpec       as Codec
import           Test.Surreal.Harness         (findSurreal, startServer, stopServer)
import qualified Test.Surreal.IntegrationSpec as Integration
import qualified Test.Surreal.RpcSpec         as Rpc
import qualified Test.Surreal.ValueSpec       as Value

main :: IO ()
main = do
  mbin <- findSurreal
  let pureTests = [Codec.tests, Value.tests, Rpc.tests]
  case mbin of
    Nothing ->
      defaultMain $ testGroup "surrealdb" $
        pureTests ++ [testCase "integration (skipped: no surreal binary)" (pure ())]
    Just bin ->
      defaultMain $ localOption (NumThreads 1) $ testGroup "surrealdb" $
        pureTests ++
        [ withResource (startServer bin) stopServer Integration.tests ]
