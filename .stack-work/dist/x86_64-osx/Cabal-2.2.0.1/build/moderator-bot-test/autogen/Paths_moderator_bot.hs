{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_moderator_bot (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/bin"
libdir     = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3/moderator-bot-0.1.0.0-21ZtKMXxRn2JjKxgAF0fHB-moderator-bot-test"
dynlibdir  = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/share/x86_64-osx-ghc-8.4.3/moderator-bot-0.1.0.0"
libexecdir = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/libexec/x86_64-osx-ghc-8.4.3/moderator-bot-0.1.0.0"
sysconfdir = "/Users/js_monk/Projects/moderator-bot/.stack-work/install/x86_64-osx/lts-12.11/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "moderator_bot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "moderator_bot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "moderator_bot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "moderator_bot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "moderator_bot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "moderator_bot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
