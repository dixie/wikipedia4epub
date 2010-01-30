module Wikipedia4epub.Commands where
import Network.Wikipedia
import Web.Firefox
import System.FilePath
import System.Directory
import Network.URL
import Control.Monad (liftM)
import Data.List (nub, foldl')
import System.IO
import Codec.EBook
import qualified Data.ByteString.Lazy as B

wiki4e_fetchArticle :: FilePath -> URL -> IO ()
wiki4e_fetchArticle oud x = do
  e <- doesFileExist filename
  if not e then do
    putStrLn $ "Fetching : " ++ (exportURL x)
    (WikiArticleHTML t c) <- fetchArticle x
    withBinaryFile filename WriteMode (\h -> hPutStr h c)
    else do
       putStrLn $ "File already exists. Skipping download: " ++ filename
  where
    filename = oud </> title
    title    = articleURL2Title x

wiki4e_fetchArticles :: FilePath -> [URL] -> IO ()
wiki4e_fetchArticles oud xs = mapM_ (wiki4e_fetchArticle oud) xs

wiki4e_fetchArticleTree :: String -> Int -> IO ()
wiki4e_fetchArticleTree xs l = undefined

wiki4e_sanitizeArticle :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticle inf ouf = do
     withFile inf ReadMode (\hi -> do
          hSetEncoding hi utf8
          withFile ouf WriteMode (\ho -> do 
                  hSetEncoding ho utf8
                  c <- hGetContents hi
                  let a = sanitizeArticle $ WikiArticleHTML "" c
                  hPutStr ho $ waContent a
               )
         )

wiki4e_sanitizeArticles :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticles ind oud = do
     xs <- wiki4e_listFiles ind
     mapM_ (\x -> wiki4e_sanitizeArticle (ind </> x) (oud </> x)) xs

wiki4e_listFiles :: FilePath -> IO [FilePath]
wiki4e_listFiles xs = liftM (filter (\(c:_) -> c /= '.')) $ getDirectoryContents xs

wiki4e_createEpub :: String -> FilePath -> IO ()
wiki4e_createEpub bookName srcDir = do
     xs <- wiki4e_listFiles srcDir
     let book = emptyBook { 
                  bookID = "http://localhost/"++bookName,
                  bookAuthor = "wiki4e-firefox-epub",
                  bookTitle = bookName
                }
     let fileNames = map (srcDir </>) xs
     items <- mapM (loadArticleFile "wiki") fileNames
     let bookFull = foldl' addItem2Book book items
     let epubFName = bookName++".epub"
     outdata <- book2Str' bookFull
     B.writeFile epubFName  outdata
     putStrLn $ epubFName ++ " constructed."

loadArticleFile :: FilePath -> FilePath -> IO BookItem
loadArticleFile bookDir fname = do
   cs <- B.readFile fname
   return (BookItem name bfile cs opsMediatype (Just (ChapterMetadata name))) 
   where
      bfile = bookDir </> name
      name = takeFileName $ normalise fname

wiki4e_listFirefoxURLs :: IO [URL]
wiki4e_listFirefoxURLs = do
     xs <- listAllHistoryURLs
     return $ nub $ filter (isArticleURL) xs
