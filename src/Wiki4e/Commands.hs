module Wiki4e.Commands where
import Network.Wikipedia
import Web.Firefox
import System.FilePath
import System.Directory
import Network.URL
import Control.Monad (liftM, filterM)
import Data.List (nub, foldl')
import Data.Char (toLower)
import System.IO
import Network.HTTP
import Codec.EBook
import qualified Data.ByteString.Lazy as STRL
import qualified Data.ByteString as STR

-- Fetching state where first number is total and second is current item.
type FetchingState = (Int,Int)
type FetchingMethod = (URL -> IO String)

fs2str :: FetchingState -> String
fs2str (total,current) = "["++(show current)++"/"++(show total)++"] " 

wiki4e_fetch :: FilePath -> FetchingMethod -> FetchingState -> URL -> IO ()
wiki4e_fetch outf fm fs x = do
  e <- doesFileExist outf
  if not e then do
    putStrLn $ (fs2str fs) ++ "Fetching : " ++ exportURL x
    c <- fm x
    withBinaryFile outf WriteMode (flip hPutStr c)
    else putStrLn $ (fs2str fs) ++ "Already cached. Skipping download. " ++ outf

wiki4e_fetchArticles :: FilePath -> [URL] -> IO ()
wiki4e_fetchArticles oud xs = mapM_ (\(st,x) -> wiki4e_fetch (nm x) fm st x) $ zip fsls xs
    where
      fsls = [ (total,i) | i <- [1..total] ]
      total = length xs
      nm x = oud </> (articleURL2Title x)
      fm x = do
        (WikiArticleHTML _ c) <- fetchArticle x
        return c

wiki4e_fetchImages :: FilePath -> [URL] -> IO ()
wiki4e_fetchImages oud xs = mapM_ (\(st,x) -> wiki4e_fetch (nm x) fm st x) $ zip fsls xs
    where
      fsls = [ (total,i) | i <- [1..total] ]
      total = length xs
      nm x = oud </> (sanitizeFileName $ takeFileName (url_path x))
      fm x = do
        rsp <- Network.HTTP.simpleHTTP (getRequest (exportURL x))
        c <- (getResponseBody rsp)
        return c

wiki4e_sanitizeArticle :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticle inf ouf = withFile inf ReadMode (\hi -> do
          hSetEncoding hi utf8
          withFile ouf WriteMode (\ho -> do 
                  hSetEncoding ho utf8
                  c <- hGetContents hi
                  forceList c `seq` hClose hi
                  let a = sanitizeArticle $ WikiArticleHTML "" c
                  hPutStr ho $ waContent a
               )
         )

wiki4e_listArticleImages :: FilePath -> IO [URL]
wiki4e_listArticleImages x = do
          h <- openFile x ReadMode
          hSetEncoding h utf8
          c <- hGetContents h
          forceList c `seq` hClose h
          return $ getArticleImages (WikiArticleHTML "" c)

wiki4e_listArticlesImages :: FilePath -> IO [URL]
wiki4e_listArticlesImages ind = do 
     xs <- wiki4e_listFiles ind
     ys <- mapM (wiki4e_listArticleImages) xs
     return $ nub $ concat ys

wiki4e_sanitizeArticles :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticles ind oud = wiki4e_listFiles ind >>= 
     mapM_ (\x -> wiki4e_sanitizeArticle x (oud </> (takeFileName x)))

wiki4e_listFiles :: FilePath -> IO [FilePath]
wiki4e_listFiles xs = do
  nonDot <- liftM (filter (\(c:_) -> c /= '.')) $ getDirectoryContents xs
  dirs <- filterM (doesDirectoryExist) (map (xs </>) nonDot)
  fils <- filterM (\e -> liftM (not) $ doesDirectoryExist e ) (map (xs </>) nonDot)
  ys <- mapM (wiki4e_listFiles) dirs
  return $ (concat ys) ++ fils

wiki4e_listFirefoxURLs :: IO [URL]
wiki4e_listFirefoxURLs = do
     xs <- listAllHistoryURLs
     return $ nub $ filter isArticleURL xs

wiki4e_crawlArticlesLinks :: FilePath -> [URL] -> Int -> IO [URL]
wiki4e_crawlArticlesLinks _ _  0 = return []
wiki4e_crawlArticlesLinks _ [] _ = return []
wiki4e_crawlArticlesLinks outd us (l + 1) = do
  wiki4e_fetchArticles outd us
  let fs = map (outd </>) $ map (articleURL2Title) us
  as <- mapM (wiki4e_readArticle) fs
  let xs = nub $ concat $ map getArticleLinksAbs as
  ys <- wiki4e_crawlArticlesLinks outd xs l
  return (nub $ us++xs++ys)

wiki4e_createEpub :: String -> FilePath -> FilePath -> IO ()
wiki4e_createEpub bookName srcDir imgDir = do
     let book = emptyBook { 
                  bookID = "http://localhost/"++bookName,
                  bookAuthor = "wiki4e-firefox-epub",
                  bookTitle = bookName
                }
     xs <- wiki4e_listFiles srcDir
     itemsA <- mapM (\(i,x) -> loadArticleFile i "wiki" x) $ zip [1..] xs
     ys <- wiki4e_listFiles imgDir
     itemsI <- mapM (\(i,x) -> loadImgFile i "wiki/img" x) $ zip [(length xs + 1)..] ys
     let bookFull = foldl' addItem2Book book (itemsA ++ itemsI)
     let epubFName = bookName++".epub"
     outdata <- book2Bin' bookFull
     STRL.writeFile epubFName  outdata
     putStrLn $ epubFName ++ " constructed."

-- Support Functions
wiki4e_initConfig :: IO (String,String,String)
wiki4e_initConfig = do
  tmpDir <- getWiki4eDir
  let tmpDirFetch    = tmpDir </> "wiki4e_fetch"     
  let tmpDirSanitize = tmpDir </> "wiki4e_sanitize"
  let tmpDirImgs     = tmpDir </> "wiki4e_images"
  createDirectoryIfMissing True tmpDirFetch
  createDirectoryIfMissing True tmpDirSanitize
  createDirectoryIfMissing True tmpDirImgs
  return (tmpDirFetch,tmpDirSanitize,tmpDirImgs)

wiki4e_readArticle :: FilePath -> IO WikiArticle
wiki4e_readArticle inf = do
  hi <- openBinaryFile inf ReadMode
  hSetEncoding hi utf8
  c <- hGetContents hi
  forceList c `seq` hClose hi
  return $ WikiArticleHTML "" c


loadArticleFile :: Int -> FilePath -> FilePath -> IO BookItem
loadArticleFile i bookDir fname = do
   cs <- STR.readFile fname
   return (BookItem aid bfile (toLazy cs) opsMediatype (Just (ChapterMetadata name))) 
   where
      aid = show i
      bfile = bookDir </> name
      name = takeFileName $ normalise fname

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
 
getWiki4eDir = getAppUserDataDirectory "wiki4e"

toLazy :: STR.ByteString -> STRL.ByteString
toLazy xs = STRL.pack $ STR.unpack xs 

forceList :: String -> String
forceList [] = []
forceList (x:xs) = forceList xs `seq` (x:xs)
