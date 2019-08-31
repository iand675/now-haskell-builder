{-# LANGUAGE ViewPatterns #-}
module ModuleScanner where

import Control.Effect
import Control.Effect.Reader
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Either (either)
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L

-- GHC lib tasks
import Config
import DynFlags
import FastString
import Fingerprint
import HeaderInfo
import HsSyn
import Module
import Lexer
import Outputable
import Pretty
import Parser
import Platform
import RdrName
import StringBuffer
import SrcLoc

import qualified Data.Map.Strict as Map
import System.IO

import System.FilePath

{-
TODO figure out how to 'flatten' GenericPackageDescription into PackageDescription
TODO figure out how to *write* package descriptions back out?
-}

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

data Analyze = Analyze
  { moduleName :: T.Text
  -- , moduleType :: ModuleType
  , functionName :: ModuleExports
  , watch :: [FilePath] -- Local modules
  }

instance ToJSON Analyze where
  toJSON (Analyze mn fn w) = object
    [ "moduleName" .= mn
    -- , "moduleType" .= mt
    , "functionName" .= fn
    , "watch" .= w
    ]

fakeSettings :: Settings
fakeSettings =
  Settings
    { sTargetPlatform = platform
    , sPlatformConstants = platformConstants
    , sProjectVersion = cProjectVersion
    , sProgramName = "ghc"
    , sOpt_P_fingerprint = fingerprint0
    , sTmpDir = "."
    }
  where
    platform =
      Platform
        { platformWordSize = 8
        , platformOS = OSUnknown
        , platformUnregisterised = True
        }
    -- This bit may need to be conditional on OS.
    platformConstants =
      PlatformConstants
        { pc_DYNAMIC_BY_DEFAULT = False
        , pc_WORD_SIZE = 8
        , pc_STD_HDR_SIZE = 1
        , pc_TAG_BITS = 3
        , pc_BLOCKS_PER_MBLOCK = 252
        , pc_BLOCK_SIZE = 4096
        , pc_MIN_PAYLOAD_SIZE = 1
        , pc_MAX_Real_Vanilla_REG = 6
        , pc_MAX_Vanilla_REG = 10
        , pc_MAX_Real_Long_REG = 0
        }



readDesiredModule :: FilePath -> IO Analyze
readDesiredModule file = do
  (flags, res) <- loadModule file
  case res of
    POk _ lm -> runM $ runReader flags $ do
      -- printForUser flags stdout neverQualify (ppr $ unLoc lm)
      r <- findMainOrHandler $ unLoc lm
      let modName = T.pack $ maybe "Main" (moduleNameString . unLoc) $ hsmodName $ unLoc lm
      pure $ Analyze modName r [] -- TODO
    _ -> error "Failed to parse module!"

mkDynFlags :: String -> String -> IO DynFlags
mkDynFlags filename s = do
  dirs_to_clean <- newIORef Map.empty
  files_to_clean <- newIORef emptyFilesToClean
  next_temp_suffix <- newIORef 0
  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
        , hscTarget = HscNothing
        , pkgDatabase = Just []
        , dirsToClean = dirs_to_clean
        , filesToClean = files_to_clean
        , nextTempSuffix = next_temp_suffix
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "module-scanner")
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags fp contents dflags0 = do
      let opts = getOptions dflags0 (stringToStringBuffer contents) fp
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

runParser :: FilePath -> DynFlags -> String -> P a -> Lexer.ParseResult a
runParser p flags str parser = unP parser parseState
  where
    filename = p
    location = mkRealSrcLoc (mkFastString filename) 1 1
    _buf = stringToStringBuffer str
    parseState = mkPState flags _buf location

loadModule :: FilePath -> IO (DynFlags, Lexer.ParseResult (Located (HsModule GhcPs)))
loadModule fp = do
  contents <- readFile fp
  flags <- mkDynFlags fp contents
  pure (flags, runParser fp flags contents parseModule)


data ModuleExports
  = ExportsMain
  | ExportsHandler
  | NoSupportedExport
  deriving (Show)

instance ToJSON ModuleExports where
  toJSON e = case e of
    ExportsMain -> String "main"
    ExportsHandler -> String "handler"
    NoSupportedExport -> Null

data ModuleType
  = Executable
  | Library
  deriving (Show)

instance ToJSON ModuleType where
  toJSON x = case x of
    Executable -> String "Executable"
    Library -> String "Library"

findMainOrHandler :: (Member (Reader DynFlags) sig, Carrier sig m, MonadIO m) => HsModule GhcPs -> m ModuleExports
findMainOrHandler m = case hsmodExports m of
  Nothing -> scanListDecls $ hsmodDecls m
  Just lExps -> case map unLoc $ unLoc lExps of
    [] -> pure NoSupportedExport
    -- TODO handle other cases
    exps -> do
      expMs <- mapM checkExport exps
      pure $ case expMs of
        (r:_) -> r
        _ -> NoSupportedExport

rdrNameCheck :: RdrName -> ModuleExports
rdrNameCheck name = if mkVarUnqual (mkFastString "main") == name
  then ExportsMain
  else if mkVarUnqual (mkFastString "handler") == name
  then ExportsHandler
  else NoSupportedExport

prettyPrintDump :: (Member (Reader DynFlags) sig, Carrier sig m, MonadIO m, Outputable a) => a -> m ()
prettyPrintDump x = do
  flags <- ask
  liftIO $ printForUser flags stdout neverQualify (ppr x)

checkExport :: (Member (Reader DynFlags) sig, Carrier sig m, MonadIO m) => IE GhcPs -> m ModuleExports
checkExport ie = do
  -- prettyPrintDump ie
  case ie of
    IEVar _ wrappedName -> case unLoc wrappedName of
      IEName name -> pure $ rdrNameCheck $ unLoc name
      IEPattern _ -> pure NoSupportedExport
      IEType _ -> pure NoSupportedExport
    IEThingAbs _ _ -> pure NoSupportedExport
    IEThingAll _ _ -> pure NoSupportedExport -- TODO
    IEThingWith _ _ _ _ _ -> pure NoSupportedExport -- TODO
    IEModuleContents _ _ -> pure NoSupportedExport -- TODO
    IEGroup _ _ _ -> pure NoSupportedExport
    IEDoc _ _ -> pure NoSupportedExport
    IEDocNamed _ _ -> pure NoSupportedExport
    XIE _ -> pure NoSupportedExport

scanListDecls :: (Member (Reader DynFlags) sig, Carrier sig m, MonadIO m) => [LHsDecl GhcPs] -> m ModuleExports
scanListDecls decls = do
  declMs <- mapM (scanDecl . unLoc) decls
  pure $ case declMs of
    [] -> NoSupportedExport
    (r:_) -> r

scanDecl :: (Member (Reader DynFlags) sig, Carrier sig m, MonadIO m) => HsDecl GhcPs -> m ModuleExports
scanDecl (ValD _ bind) = case bind of
  (FunBind _ fid _ _ _) -> do
    -- prettyPrintDump fid
    pure $ rdrNameCheck $ unLoc fid
  _ -> pure NoSupportedExport
scanDecl _ = pure NoSupportedExport
