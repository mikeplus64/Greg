module Paths_GREG_WARRIOR_PRINCESS (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,6], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/opuk/.cabal/bin"
libdir     = "/home/opuk/.cabal/lib/GREG-WARRIOR-PRINCESS-0.6/ghc-7.4.1"
datadir    = "/home/opuk/.cabal/share/GREG-WARRIOR-PRINCESS-0.6"
libexecdir = "/home/opuk/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "GREG_WARRIOR_PRINCESS_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GREG_WARRIOR_PRINCESS_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GREG_WARRIOR_PRINCESS_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GREG_WARRIOR_PRINCESS_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
