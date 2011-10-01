module Wiki4e.Commands.Common ( Wiki4eConfig(..)
                              , wiki4e_initConfig
                              , readFileUTF8
                              , toLazy
                              , forceList ) 
where
import System.FilePath
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as STRL
import qualified Data.ByteString as STR

-- | Basic configuration of the commands
data Wiki4eConfig = Wiki4eConfig { w4confDirFetch      :: FilePath,
                                   w4confDirImg        :: FilePath,
                                   w4confDirSanitized  :: FilePath, 
                                   w4confDomain        :: String }

-- Support Functions
getWiki4eDir :: IO FilePath
getWiki4eDir = getAppUserDataDirectory "wiki4e"

wiki4e_initConfig :: String -> IO Wiki4eConfig
wiki4e_initConfig domain = do
  tmpDir <- getWiki4eDir
  let tmpDirFetch    = tmpDir </> "wiki4e_fetch"     
  let tmpDirSanitize = tmpDir </> "wiki4e_sanitize"
  let tmpDirImgs     = tmpDir </> "wiki4e_images"
  createDirectoryIfMissing True tmpDirFetch
  createDirectoryIfMissing True tmpDirSanitize
  createDirectoryIfMissing True tmpDirImgs
  return (Wiki4eConfig tmpDirFetch tmpDirImgs tmpDirSanitize domain)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 x = do
          h <- openFile x ReadMode
          hSetEncoding h utf8
          c <- hGetContents h
          forceList c `seq` hClose h
          return c

toLazy :: STR.ByteString -> STRL.ByteString
toLazy xs = STRL.pack $ STR.unpack xs 

forceList :: String -> String
forceList [] = []
forceList (x:xs) = forceList xs `seq` (x:xs)
