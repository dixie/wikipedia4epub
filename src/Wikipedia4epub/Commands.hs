module Wikipedia4epub.Commands where
import Network.Wikipedia
import Web.Firefox
import System.FilePath
import System.Directory
import Network.URL
import Control.Monad (liftM, filterM)
import Data.List (nub, foldl')
import Data.Char (isLetter)
import System.IO
import Network.HTTP
import Data.Maybe
import Codec.EBook
import Debug.Trace
import qualified Data.ByteString.Lazy as B

wiki4e_fetchArticle :: FilePath -> URL -> IO ()
wiki4e_fetchArticle oud x = do
  e <- doesFileExist filename
  if not e then do
    putStrLn $ "Fetching : " ++ exportURL x
    (WikiArticleHTML t c) <- fetchArticle x
    withBinaryFile filename WriteMode (flip hPutStr c)
    else putStrLn $ "File already exists. Skipping download: " ++ filename
  where
    filename = oud </> title
    title    = articleURL2Title x

wiki4e_fetchArticles :: FilePath -> [URL] -> IO ()
wiki4e_fetchArticles oud xs = mapM_ (wiki4e_fetchArticle oud) xs

wiki4e_fetchArticleTree :: String -> Int -> IO ()
wiki4e_fetchArticleTree xs l = undefined

wiki4e_sanitizeArticle :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticle inf ouf = withFile inf ReadMode (\hi -> do
          hSetEncoding hi utf8
          withFile ouf WriteMode (\ho -> do 
                  hSetEncoding ho utf8
                  c <- hGetContents hi
                  let a = sanitizeArticle $ WikiArticleHTML "" c
                  hPutStr ho $ waContent a
               )
         )

wiki4e_listArticleImages :: FilePath -> IO [URL]
wiki4e_listArticleImages x = do
          h <- openFile x ReadMode
          hSetEncoding h utf8
          c <- hGetContents h
          return $ getArticleImages (WikiArticleHTML "" c)

wiki4e_listArticlesImages :: FilePath -> IO [URL]
wiki4e_listArticlesImages ind = do 
     xs <- wiki4e_listFiles ind
     ys <- mapM (wiki4e_listArticleImages) xs
     return $ nub $ concat ys

wiki4e_fetchImage :: FilePath -> URL -> IO ()
wiki4e_fetchImage oud x = do
  e <- doesFileExist filename
  if not e then do
    createDirectoryIfMissing True (takeDirectory filename)
    putStrLn $ "Fetching : " ++ exportURL x
    rsp <- Network.HTTP.simpleHTTP (getRequest (exportURL x))
    c <- (getResponseBody rsp)
    withBinaryFile filename WriteMode (flip hPutStr c)
    else putStrLn $ "File already exists. Skipping download: " ++ filename
  where
    filename = oud </> name
    name     = sanitizeFileName $ takeFileName (url_path x)

wiki4e_fetchImages :: FilePath -> [URL] -> IO ()
wiki4e_fetchImages oud xs = mapM_ (wiki4e_fetchImage oud) xs

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
     B.writeFile epubFName  outdata
     putStrLn $ epubFName ++ " constructed."

-- Support Functions

loadArticleFile :: Int -> FilePath -> FilePath -> IO BookItem
loadArticleFile i bookDir fname = do
   cs <- B.readFile fname
   return (BookItem aid bfile cs opsMediatype (Just (ChapterMetadata name))) 
   where
      aid = show i
      bfile = bookDir </> name
      name = takeFileName $ normalise fname

loadImgFile :: Int -> FilePath -> FilePath -> IO BookItem
loadImgFile i bookDir fname = do
   cs <- B.readFile fname
   return (BookItem aid bfile cs mimeType Nothing) 
   where
      mimeType | takeExtension name == ".png"  = "image/png"
               | takeExtension name == ".jpeg" = "image/jpeg"
               | takeExtension name == ".jpg"  = "image/jpeg"
               | takeExtension name == ".gif"  = "image/gif"
               | otherwise = "image/png"
      aid = show i
      bfile = bookDir </> name
      name = takeFileName $ normalise fname
