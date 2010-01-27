module Wikipedia4epub.Commands where
import Network.Wikipedia
import Web.Firefox
import System.FilePath ((</>))
import Network.URL
import Data.List (nub)
import System.IO

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
wiki4e_sanitizeArticle ind oud = undefined

wiki4e_createEpub :: String -> IO ()
wiki4e_createEpub = undefined

wiki4e_listFirefoxURLs :: IO [URL]
wiki4e_listFirefoxURLs = do
     xs <- listAllHistoryURLs
     return $ nub $ filter (isArticleURL) xs
