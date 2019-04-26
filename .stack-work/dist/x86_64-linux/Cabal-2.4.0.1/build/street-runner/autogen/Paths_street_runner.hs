{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
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

bindir     = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/bin"
libdir     = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/lib/x86_64-linux-ghc-8.6.4/street-runner-0.0.0-IRHS6dLHshlHGWQD6b0861-street-runner"
dynlibdir  = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/share/x86_64-linux-ghc-8.6.4/street-runner-0.0.0"
libexecdir = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/libexec/x86_64-linux-ghc-8.6.4/street-runner-0.0.0"
sysconfdir = "/home/aakash/workspace/codes/street_runner_git/.stack-work/install/x86_64-linux/lts-13.15/8.6.4/etc"

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
