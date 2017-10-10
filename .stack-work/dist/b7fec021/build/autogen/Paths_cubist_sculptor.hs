{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_cubist_sculptor (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\cubist-sculptor\\.stack-work\\install\\d9016aaf\\bin"
libdir     = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\cubist-sculptor\\.stack-work\\install\\d9016aaf\\lib\\x86_64-windows-ghc-8.0.1\\cubist-sculptor-0.1.0.0-6RGwNgOrlKp6CP8r4nQqVY"
datadir    = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\cubist-sculptor\\.stack-work\\install\\d9016aaf\\share\\x86_64-windows-ghc-8.0.1\\cubist-sculptor-0.1.0.0"
libexecdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\cubist-sculptor\\.stack-work\\install\\d9016aaf\\libexec"
sysconfdir = "C:\\Users\\Jonatan\\Desktop\\Haskell\\modules\\cubist-sculptor\\.stack-work\\install\\d9016aaf\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cubist_sculptor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cubist_sculptor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cubist_sculptor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cubist_sculptor_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cubist_sculptor_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
