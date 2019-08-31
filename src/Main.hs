module Main where

import CabalScanner

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory
import System.Environment




main :: IO ()
main = do
  (basePath:_) <- getArgs
  isDir <- doesDirectoryExist basePath
  out <- if isDir
    then do
      gpd <- getCabalPackage basePath
      packInfo <- performCabalPackageScan gpd
      -- TODO useful things here
      pure $ PackageUsedOutput packInfo
    else do
      scan <- dispatchFileScan basePath
      case scan of
        Left modInfo -> do
          case modInfo of
            -- (ModuleInfo (InPackageLibrary fp) analysis) ->
            _ -> return ()
          pure $ ModuleUsedOutput modInfo
        Right packInfo -> do
          -- TODO useful things here
          pure $ PackageUsedOutput packInfo
  L.putStrLn $ encodePretty out
