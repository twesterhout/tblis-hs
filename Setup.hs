module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
  ( InstallDirs (..),
    LocalBuildInfo (..),
    absoluteInstallDirs,
    localPkgDescr,
  )
import Distribution.Simple.Setup
import Distribution.Simple.Utils
  ( installMaybeExecutableFile,
    notice,
    rawSystemExit,
  )
import System.Directory (doesDirectoryExist, findFilesWith, getCurrentDirectory, listDirectory)

main :: IO ()
main = defaultMainWithHooks $ autoconfUserHooks {postCopy = copyLibTblis}

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags localBuildInfo libPref = do
  notice verbosity $ "Copying TBLIS C library..."
  print =<< getCurrentDirectory
  let cabalBuildDir = buildDir localBuildInfo
  files <- filter (isPrefixOf "tblis-") <$> listDirectory cabalBuildDir
  forM_ files $ \file -> do
    let prefix = cabalBuildDir <> "/" <> file
    hasLib <- doesDirectoryExist (prefix <> "/lib")
    hasInclude <- doesDirectoryExist (prefix <> "/include")
    when (hasLib && hasInclude) $
      forM_ ["libtblis.a", "libtci.a"] $ \f ->
        installMaybeExecutableFile verbosity (prefix <> "/lib/" <> f) (libPref <> "/" <> f)
  where
    -- print files
    -- print $ buildDir localBuildInfo
    -- libDir <- (<> "/third_party/install/lib") <$> getCurrentDirectory
    -- let libDir = "lib"
    -- forM_ ["libtblis.a", "libtci.a"] $ \f ->
    --   installMaybeExecutableFile verbosity (libDir <> "/" <> f) (libPref <> "/" <> f)

    verbosity = fromFlag $ configVerbosity flags

copyLibTblis :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibTblis _ flags packageDescription localBuildInfo =
  unless (getCabalFlag "" config) $ copyLib config localBuildInfo libPref
  where
    libPref = libdir . absoluteInstallDirs packageDescription localBuildInfo . fromFlag . copyDest $ flags
    config = configFlags localBuildInfo

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
  where
    allFlags = configConfigurationsFlags flags
    name' = map toLower name
