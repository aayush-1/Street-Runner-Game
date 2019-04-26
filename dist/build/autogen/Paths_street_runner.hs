{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_street_runner (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/aayush/.cabal/bin"
libdir     = "/home/aayush/.cabal/lib/x86_64-linux-ghc-8.0.2/street-runner-0.0.0-Emc2sgD8ESKLK08hrR9Imt"
dynlibdir  = "/home/aayush/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/aayush/.cabal/share/x86_64-linux-ghc-8.0.2/street-runner-0.0.0"
libexecdir = "/home/aayush/.cabal/libexec"
sysconfdir = "/home/aayush/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "street_runner_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "street_runner_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "street_runner_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "street_runner_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "street_runner_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "street_runner_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
