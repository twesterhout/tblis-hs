module Main (main) where

import Control.Monad (forM_, unless)
import Data.Char (toLower)
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
import System.Directory (getCurrentDirectory)

main :: IO ()
main = defaultMainWithHooks $ autoconfUserHooks {postCopy = copyLibTblis}

copyLib :: ConfigFlags -> LocalBuildInfo -> FilePath -> IO ()
copyLib flags localBuildInfo libPref = do
  notice verbosity $ "Copying TBLIS C library..."
  libDir <- (<> "/cbits/") <$> getCurrentDirectory
  forM_ ["libtblis.a", "libtci.a"] $ \f ->
    installMaybeExecutableFile verbosity (libDir <> "/" <> f) (libPref <> "/" <> f)
  where
    verbosity = fromFlag $ configVerbosity flags

copyLibTblis :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyLibTblis _ flags packageDescription localBuildInfo = copyLib config localBuildInfo libPref
  where
    libPref = libdir . absoluteInstallDirs packageDescription localBuildInfo . fromFlag . copyDest $ flags
    config = configFlags localBuildInfo

getCabalFlag :: String -> ConfigFlags -> Bool
getCabalFlag name flags = fromMaybe False (lookupFlagAssignment (mkFlagName name') allFlags)
  where
    allFlags = configConfigurationsFlags flags
    name' = map toLower name