module Wiki4e.Commands.Images ( wiki4e_fetchImages
                              , wiki4e_getImgFiles
                              , imgURL2File
                              , loadImgFile ) 
where
import System.FilePath
import Network.URL
import Data.Char (toLower)
import Network.HTTP
import Codec.EBook
import qualified Data.ByteString as STR
import Network.Wikipedia
import Wiki4e.Commands.Common
import Wiki4e.Commands.Fetching

wiki4e_fetchImages :: Wiki4eConfig -> [URL] -> IO ()
wiki4e_fetchImages config xs = mapM_ (\(st,x) -> wiki4e_fetch (nm x) fm st x) $ zip fsls xs
    where
      fsls = [ (total,i) | i <- [1..total] ]
      total = length xs
      nm x = imgURL2File config x
      fm x = do
        rsp <- Network.HTTP.simpleHTTP (getRequest (exportURL x))
        c <- (getResponseBody rsp)
        return c

loadImgFile :: Int -> FilePath -> FilePath -> IO BookItem
loadImgFile i bookDir fname = do
   cs <- STR.readFile fname
   return (BookItem aid bfile (toLazy cs) mimeType Nothing) 
   where
      mimeType | hasExt ".png"  name = "image/png"
               | hasExt ".jpeg" name = "image/jpeg"
               | hasExt ".jpg"  name = "image/jpeg"
               | hasExt ".gif"  name = "image/gif"
               | otherwise = "image/png"
      hasExt x f = (map toLower $ takeExtension f) == x
      aid = show i
      bfile = bookDir </> name
      name = takeFileName $ normalise fname
 

imgURL2File :: Wiki4eConfig -> URL -> FilePath
imgURL2File config x = (w4confDirImg config) </> (sanitizeFileName $ takeFileName (url_path x))

wiki4e_getImgFiles :: Wiki4eConfig -> [URL] -> [FilePath]
wiki4e_getImgFiles config xs = map (imgURL2File config) xs
