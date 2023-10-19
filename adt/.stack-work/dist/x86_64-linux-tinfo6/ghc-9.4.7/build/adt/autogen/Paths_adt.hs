{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_adt (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/bin"
libdir     = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/lib/x86_64-linux-ghc-9.4.7/adt-0.1.0.0-7Y1VCPeayVZAX3F5tvLX0V-adt"
dynlibdir  = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/share/x86_64-linux-ghc-9.4.7/adt-0.1.0.0"
libexecdir = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/libexec/x86_64-linux-ghc-9.4.7/adt-0.1.0.0"
sysconfdir = "/home/chaos/Desktop/projects/hs/desert_storm/adt/.stack-work/install/x86_64-linux-tinfo6/7b65328b8748e9debcb6ee6bc32354c62ceda50b306a4d464dfd22d91e496e20/9.4.7/etc"

getBinDir     = catchIO (getEnv "adt_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "adt_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "adt_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "adt_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "adt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "adt_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
