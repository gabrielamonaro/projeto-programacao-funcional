{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_projetodamas (
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
bindir     = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/bin"
libdir     = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/lib/x86_64-linux-ghc-9.6.2/projetodamas-0.1.0.0-35Ye3kGV0O113QgymFzBOd-projetodamas-exe"
dynlibdir  = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/lib/x86_64-linux-ghc-9.6.2"
datadir    = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/share/x86_64-linux-ghc-9.6.2/projetodamas-0.1.0.0"
libexecdir = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/libexec/x86_64-linux-ghc-9.6.2/projetodamas-0.1.0.0"
sysconfdir = "/home/gabriela/Documents/haskell/projeto-programacao-funcional/.stack-work/install/x86_64-linux-tinfo6/abfd00bb9673023b92a25f2d0fb8129f3a52613f36f3a47beba9c6365a6acb02/9.6.2/etc"

getBinDir     = catchIO (getEnv "projetodamas_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "projetodamas_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "projetodamas_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "projetodamas_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projetodamas_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projetodamas_sysconfdir") (\_ -> return sysconfdir)



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
