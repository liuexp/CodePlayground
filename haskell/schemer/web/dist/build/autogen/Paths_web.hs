module Paths_web (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/liuexp/.cabal/bin"
libdir     = "/home/liuexp/.cabal/lib/web-0.1/ghc-7.0.3"
datadir    = "/home/liuexp/.cabal/share/web-0.1"
libexecdir = "/home/liuexp/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "web_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "web_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "web_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "web_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
