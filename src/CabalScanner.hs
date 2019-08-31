module CabalScanner where

import Data.Aeson
import qualified Data.Text as T

-- Cabal things
import Distribution.Verbosity (normal)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription) -- TODO replace with finalizePD
import Distribution.PackageDescription.Parsec
import Distribution.Types.UnqualComponentName
import Distribution.ModuleName (ModuleName, fromComponents)
import Distribution.Simple.Utils

import Data.List (inits)
import System.Directory
import System.Environment
import System.FilePath
import qualified ModuleScanner as Mod

data Output
  = PackageUsedOutput PackageInfo
  | ModuleUsedOutput ModuleInfo

instance ToJSON Output where
  toJSON o = case o of
    PackageUsedOutput p -> object
      [ "type" .= ("package" :: T.Text)
      , "data" .= p
      ]
    ModuleUsedOutput m -> object
      [ "type" .= ("module" :: T.Text)
      , "data" .= m
      ]

data ModulePackage
  = Dangling
  | InPackageLibrary FilePath
  | InPackageExecutable
    String -- ^ Executable name
    FilePath
  | InOtherPackageStanza FilePath

data ModuleInfo = ModuleInfo
  { moduleInfoPackage :: ModulePackage
  , moduleInfoAnalysis :: Mod.Analyze
  }

instance ToJSON ModuleInfo where
  toJSON (ModuleInfo p a) = object
    [ "package" .= case p of
        Dangling -> Null
        InPackageLibrary fp -> object ["library" .= (takeBaseName fp), "path" .= fp]
        InPackageExecutable e fp -> object ["executable" .= e, "path" .= fp]
        InOtherPackageStanza fp -> object ["path" .= fp]
    , "analysis" .= a
    ]

data PackageInfo = PackageInfo
  { packageInfoExecutables :: [String]
  }

instance ToJSON PackageInfo where
  toJSON (PackageInfo exes) = object
    [ "executables" .= exes
    {-
      From cabal:
        executables[].modulePath :: FilePath
        resolve libraries[].modules with buildInfo
        dataFiles :: [FilePath]
        dataDir :: FilePath
        extraSrcFiles :: [FilePath]
        extraTmpFiles :: [FilePath]
        extraDocFiles :: [FilePath]
    -}
    , "files" .= object
      [ "executableModules" .= ([] :: [FilePath])
      ]
    ]

dispatchFileScan :: FilePath -> IO (Either ModuleInfo PackageInfo)
dispatchFileScan fp = case takeFileName fp of
  "package.yml" -> error "hpack package.yml"
  "package.yaml" -> error "hpack package.yaml"
  other -> case takeExtension other of
    ".hs" -> Left <$> performHaskellModuleScan fp
    ".lhs" -> Left <$> performHaskellModuleScan fp
    -- TODO
    -- "chs" -> _
    ".cabal" -> Right <$> (produceFlatPackage fp >>= performCabalPackageScan)
    unsupported -> error ("." ++ unsupported ++ " file extension is not a supported Haskell builder type.")

performHaskellModuleScan :: FilePath -> IO ModuleInfo
performHaskellModuleScan relFp = do
  fp <- canonicalizePath relFp
  mCabalFile <- findClosestAncestorCabalFile fp
  analysis <- Mod.readDesiredModule relFp

  case mCabalFile of
    Nothing -> do
      pure $ ModuleInfo Dangling analysis
    Just cf -> do
      flatPack <- produceFlatPackage cf
      case library flatPack of
        Nothing -> do
          pure $ ModuleInfo (InOtherPackageStanza cf) analysis
        Just l -> do
          let relativeModulePath = makeRelative (takeDirectory cf) fp
              desiredModule = fileToModule (hsSourceDirs $ libBuildInfo l) relativeModulePath
          if desiredModule `isExposedFromLibrary` l
            then do
              pure $ ModuleInfo (InPackageLibrary cf) analysis
            else error "TODO: not sure what to do here..."

getCabalPackage :: FilePath -> IO PackageDescription
getCabalPackage p = do
  efp <- findPackageDesc p
  let fp = either error id efp
  produceFlatPackage fp

produceFlatPackage :: FilePath -> IO PackageDescription
produceFlatPackage fp = do
  gpd <- readGenericPackageDescription normal fp
  pure $ flattenPackageDescription gpd

performCabalPackageScan :: PackageDescription -> IO PackageInfo
performCabalPackageScan gpd = do
  let exes = map (unUnqualComponentName . exeName) $ executables gpd
  pure $ PackageInfo exes

-- Takes a path and searches for a cabal file up the directory hierarchy. returns a cabal file if found
findClosestAncestorCabalFile :: FilePath -> IO (Maybe FilePath)
findClosestAncestorCabalFile fp = do
  isDir <- doesDirectoryExist fp
  let startingDir = if isDir then fp else takeDirectory fp
  dir <- canonicalizePath startingDir
  let chunkedDir = splitPath dir
      allPaths = map joinPath $ reverse $ tail $ inits chunkedDir
  -- print allPaths
  go allPaths
  where
    go [] = pure Nothing
    go (path:ps) = do
      res <- findPackageDesc path
      case res of
        Left _ -> go ps
        Right p -> pure $ Just p


fileToModule
  :: [FilePath] -- Search paths
  -- -> [FilePath] -- Extensions
  -> FilePath -- Haskell File
  -> ModuleName
fileToModule searchPs startP = go searchPs
  where
    extensionless = dropExtension startP
    go [] = fromComponents $ splitDirectories extensionless
    go (p:ps) = let rel = makeRelative p extensionless in
      if rel == extensionless
      then go ps
      else fromComponents $ splitDirectories rel

isExposedFromLibrary :: ModuleName -> Library -> Bool
isExposedFromLibrary m l = m `elem` exposedModules l
