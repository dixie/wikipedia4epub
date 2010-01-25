import Text.HTML.TagSoup
import System.Environment
import Web.Firefox
import Data.List (nub)
import Network.Wikipedia
import Network.URL
import System.FilePath ((</>))
import System.IO 

main = do
  xs <- listAllHistoryURLs
  mapM_ (downloadArticleI) (nub $ filter (isArticleURL) xs)
  putStrLn "Done."

downloadArticleI x = do
   putStrLn $ "Fetching : " ++ (exportURL x)
   (WikiArticleHTML t c) <- fetchArticle x
   let filename = "output" </> (t ++ ".html") 
   withBinaryFile filename WriteMode (\h -> hPutStr h c)
