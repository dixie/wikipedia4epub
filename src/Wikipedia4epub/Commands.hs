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
  putStrLn $ "Fetching : " ++ (exportURL x)
  (WikiArticleHTML t c) <- fetchArticle x
  let filename = oud </> t
  withBinaryFile filename WriteMode (\h -> hPutStr h c)
  
wiki4e_fetchArticles :: FilePath -> [URL] -> IO ()
wiki4e_fetchArticles oud xs = mapM_ (wiki4e_fetchArticle oud) xs

wiki4e_fetchArticleTree :: String -> Int -> IO ()
wiki4e_fetchArticleTree xs l = undefined

wiki4e_sanitizeArticle :: FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticle ind oud = do
     xs <- wiki4e_listFiles ind
     mapM_ (\x -> copyFile (ind </> x) (oud </> x)) xs

wiki4e_listFiles :: FilePath -> IO [FilePath]
wiki4e_listFiles xs = liftM (filter (\(c:_) -> c /= '.')) $ getDirectoryContents xs

wiki4e_createEpub :: String -> FilePath -> FilePath -> IO ()
wiki4e_createEpub bookName srcDir bookDir = do
     xs <- wiki4e_listFiles srcDir
     let book = emptyBook { 
                  bookID = "http://localhost/"++bookName,
                  bookAuthor = "wiki4e-firefox-epub",
                  bookTitle = bookName
                }
     let fileNames = map (srcDir </>) xs
     items <- mapM loadItems fileNames
     let bookFull = foldl' addItem2Book book items
     let epubFName = bookName++".epub"
     outdata <- book2Str' bookFull
     B.writeFile epubFName  outdata
     putStrLn $ epubFName ++ " constructed."

loadItems :: FilePath -> IO BookItem
loadItems p = do
   c <- B.readFile p
   return (BookItem ("http://localhost/"++np) np c opsMediatype (Just (ChapterMetadata np))) 
   where
      np = normalise p

wiki4e_listFirefoxURLs :: IO [URL]
wiki4e_listFirefoxURLs = do
     xs <- listAllHistoryURLs
     return $ nub $ filter (isArticleURL) xs
