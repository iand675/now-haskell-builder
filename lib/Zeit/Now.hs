{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zeit.Now
  ( NowInput(..)
  , NowOutput(..)
  , NowOutputBody(..)
  , EventHandler
  , runloop
  , module Network.HTTP.Types.Header
  , module Network.HTTP.Types.Method
  , module Network.HTTP.Types.Status
  ) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import AWSLambdaRuntime
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Stack
import System.Environment
import Data.ByteArray.Encoding
import Network.HTTP.Client
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status
import Network.Wai.Internal
-- import Text.Show.Pretty

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


postRuntimeError :: Manager -> AWSLambdaRuntimeConfig -> AwsRequestId -> SomeException -> IO ()
postRuntimeError man conf req e =
  void $ do
    let cs = callStack
    strTrace <- case getCallStack cs of
      [] -> map T.pack <$> whoCreated e
      items -> pure $ T.lines $ T.pack $ prettyCallStack cs
    void $ dispatchLbs man conf $
      setBodyParam
        (runtimeInvocationAwsRequestIdErrorPost (ContentType MimeJSON) req) $ Body $ A.toJSON $
          ErrorRequest
          { errorRequestErrorMessage = Just $ T.pack $ show e
          , errorRequestErrorType = Nothing
          , errorRequestStackTrace = Just strTrace
          }
    -- print resp

runloop :: EventHandler NowInput NowOutput -> IO ()
runloop h = do
  man <- newManager defaultManagerSettings
  __conf <- newConfig
  apiEndpoint <- getEnv "AWS_LAMBDA_RUNTIME_API"
  let conf = __conf { configHost = "http://" <> L.pack apiEndpoint <> hostPath }
  forever $ do
    nextResp <- dispatchLbs man conf runtimeInvocationNextGet
    let (Just reqIdHdr) = lookup "Lambda-Runtime-Aws-Request-Id" $ responseHeaders nextResp
    let reqId = AwsRequestId $ T.decodeUtf8 reqIdHdr
    let dec = do
          outer <- A.eitherDecode' $ responseBody nextResp
          decodeInvocation outer
          -- pPrint $ responseHeaders nextResp
    handle (postRuntimeError man conf reqId) $ do
      let ok = either error id dec
      b <- h ok
      void $ dispatchLbs man conf $ setBodyParam (runtimeInvocationAwsRequestIdResponsePost (ContentType MimeJSON) reqId) (Body $ A.toJSON b)
    -- print respResp

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

data NowOutputBody
  = TextBody !T.Text
  | BytesBody !ByteString

-- newtype Reversed f a = Reversed (f a)

data NowOutput = NowOutput
  { nowOutputStatus :: Status
  , nowOutputHeaders :: !(H.HashMap HeaderName ByteString)
  -- , nowOutputMultiValueHeaders :: !(H.HashMap T.Text (Reversed [] T.Text))
  , nowOutputBody :: !NowOutputBody
  }

instance A.ToJSON NowOutput where
  toJSON (NowOutput s hs b) = A.object $ concat [statusFields, bodyFields, ["headers" A..= convertedHeaders]]
    where
      -- TODO can this be nicer?
      convertedHeaders = H.fromList . map (\(k, v) -> (T.decodeUtf8 $ CI.foldedCase k, T.decodeUtf8 v)) $ H.toList hs
      statusFields =
        [ "statusCode" A..= statusCode s
        , "statusDescription" A..= (T.decodeUtf8 $ statusMessage s)
        ]
      bodyFields = case b of
        TextBody t ->
          [ "body" A..= t
          , "isBase64Encoded" A..= False
          ]
        BytesBody bs ->
          [ "body" A..= T.decodeUtf8 (convertToBase Base64 bs)
          , "isBase64Encoded" A..= True
          ]
