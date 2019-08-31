{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( handler
    ) where

import qualified Data.HashMap.Strict as H
import Zeit.Now
import System.Environment

handler :: EventHandler NowInput NowOutput
handler i = do
  getEnvironment >>= print
  print i
  return $ NowOutput
    { nowOutputStatus = ok200
    , nowOutputHeaders = H.empty
    , nowOutputBody = TextBody "<html><body><h1>What goes out?</h1></body></html>"
    }
