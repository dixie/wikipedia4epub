module Wiki4e.Commands.Common ( Wiki4eConfig(..)
                              , wiki4e_initConfig
                              , toLazy
                              , forceList ) 
where
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as STRL
import qualified Data.ByteString as STR

-- | Basic configuration of the commands
data Wiki4eConfig = Wiki4eConfig { w4confDirFetch      :: FilePath,
                                   w4confDirImg        :: FilePath,
                                   w4confDirSanitized  :: FilePath }

-- Support Functions
getWiki4eDir :: IO FilePath
getWiki4eDir = getAppUserDataDirectory "wiki4e"

wiki4e_initConfig :: IO Wiki4eConfig
wiki4e_initConfig = do
  tmpDir <- getWiki4eDir
  let tmpDirFetch    = tmpDir </> "wiki4e_fetch"     
  let tmpDirSanitize = tmpDir </> "wiki4e_sanitize"
  let tmpDirImgs     = tmpDir </> "wiki4e_images"
  createDirectoryIfMissing True tmpDirFetch
  createDirectoryIfMissing True tmpDirSanitize
  createDirectoryIfMissing True tmpDirImgs
  return (Wiki4eConfig tmpDirFetch tmpDirImgs tmpDirSanitize)

toLazy :: STR.ByteString -> STRL.ByteString
toLazy xs = STRL.pack $ STR.unpack xs 

forceList :: String -> String
forceList [] = []
forceList (x:xs) = forceList xs `seq` (x:xs)
