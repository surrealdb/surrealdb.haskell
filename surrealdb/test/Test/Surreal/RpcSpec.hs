{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the RPC envelope: request encoding and frame classification.
module Test.Surreal.RpcSpec (tests) where

import qualified Data.Map.Strict      as Map
import qualified Data.UUID.Types      as UUID
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Surreal.Error (RpcError (..))
import           Database.Surreal.RPC
import           Database.Surreal.Types

tests :: TestTree
tests = testGroup "RPC"
  [ testCase "request encodes id, method and params" $
      case requestToValue (RpcRequest 7 "select" [VString "person"]) of
        VObject o -> do
          Map.lookup "id" o     @?= Just (VInt 7)
          Map.lookup "method" o @?= Just (VString "select")
          Map.lookup "params" o @?= Just (VArray (pure (VString "person")))
        other -> assertFailure ("expected object, got " ++ show other)

  , testCase "response frame is parsed by id" $
      let frame = VObject (Map.fromList [("id", VInt 3), ("result", VString "ok")])
      in parseFrame frame @?= Right (FrameResponse (RpcResponse 3 (Right (VString "ok"))))

  , testCase "error frame is parsed" $
      let err   = VObject (Map.fromList [("code", VInt 1), ("message", VString "boom")])
          frame = VObject (Map.fromList [("id", VInt 4), ("error", err)])
      in parseFrame frame @?= Right (FrameResponse (RpcResponse 4 (Left (RpcError 1 "boom"))))

  , testCase "live notification is parsed" $
      let qid    = Uuid UUID.nil
          notif  = VObject (Map.fromList
                     [ ("id", VUuid qid)
                     , ("action", VString "CREATE")
                     , ("result", VObject (Map.fromList [("x", VInt 1)]))
                     ])
          frame  = VObject (Map.fromList [("result", notif)])
      in case parseFrame frame of
           Right (FrameNotification n) -> do
             ntfQueryId n @?= qid
             ntfAction n  @?= ActionCreate
           other -> assertFailure ("expected notification, got " ++ show other)
  ]
