{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import qualified Data.Aeson as A
import AWSLambdaRuntime
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import Network.HTTP.Client
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method (Method)
import Network.Wai.Internal
import Text.Show.Pretty

type EventHandler i o = i -> IO o

hostPath :: L.ByteString
hostPath = "/2018-06-01"

data NowInput = NowInput
  { nowInputMethod :: Method
  , nowInputHost :: T.Text
  , nowInputPath :: T.Text
  , nowInputHeaders :: H.HashMap HeaderName ByteString
  , nowInputEncoding :: T.Text
  , nowInputBody :: T.Text
  } deriving (Show, Eq)

instance A.FromJSON NowInput where
  parseJSON = A.withObject "NowInput" $ \o -> do
    nowInputMethod <- T.encodeUtf8 <$> o A..: "method"
    nowInputHost <- o A..: "host"
    nowInputPath <- o A..: "path"
    textHeaders' <- o A..: "headers"
    let nowInputHeaders = H.fromList $
          map (\(k, v) ->
                 ( CI.mk $ T.encodeUtf8 k
                 , T.encodeUtf8 v
                 )) $ H.toList textHeaders'
    nowInputEncoding <- o A..: "encoding"
    nowInputBody <- o A..: "body"
    pure $ NowInput{..}

type NowOutput = A.Value

runloop :: EventHandler NowInput NowOutput -> IO ()
runloop h = do
  man <- newManager defaultManagerSettings
  __conf <- newConfig
  apiEndpoint <- getEnv "AWS_LAMBDA_RUNTIME_API"
  let conf = __conf { configHost = "http://" <> L.pack apiEndpoint <> hostPath }
  forever $ do
    nextResp <- dispatchLbs man conf runtimeInvocationNextGet
    -- Input and output limit is 6MB
    L.putStrLn $ responseBody nextResp
    let (Just reqIdHdr) = lookup "Lambda-Runtime-Aws-Request-Id" $ responseHeaders nextResp
    let dec = do
          outer <- A.eitherDecode' $ responseBody nextResp
          decodeInvocation outer
    let ok = either error id dec
    pPrint $ responseHeaders nextResp
    pPrint (ok :: Invocation NowInput)
    b <- h $ invocationBody ok
    respResp <- dispatchLbs man conf $ setBodyParam (runtimeInvocationAwsRequestIdResponsePost (ContentType MimeJSON) (AwsRequestId $ T.decodeUtf8 reqIdHdr)) (Body b)
    print respResp

type Action = T.Text

data Invocation a = Invocation
  { invocationAction :: !Action
  , invocationBody :: a
  } deriving (Show, Eq)

instance A.FromJSON a => A.FromJSON (Invocation a) where
  parseJSON = A.withObject "Invocation" $ \o -> do
    invocationAction <- o A..: "Action"
    invocationBody <- o A..: "body"
    pure $ Invocation{..}

decodeInvocation :: A.FromJSON a => Invocation T.Text -> Either String a
decodeInvocation = A.eitherDecodeStrict' . T.encodeUtf8 . invocationBody

someFunc :: IO ()
someFunc = do
  getEnvironment >>= print
  runloop $ \i -> do
    print i
    return $ A.object
      [ "statusCode" A..= (200 :: Int)
      , "statusDescription" A..= ("OK" :: T.Text)
      , "headers" A..= (A.object [])
      , "multiValueHeaders" A..= (A.object [])
      , "body" A..= ("Noodle" :: T.Text)
      , "isBase64Encoded" A..= False
      ]
