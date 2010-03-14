import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Data.List (nub)
import System.IO 
import Network.URL
import Wiki4e.Commands
import Network.Wikipedia (isArticleURL)
import Web.Firefox

defaultEbookName = "Wikipedia_Articles_From_Firefox"

main = do
  name <- getProgName
  args <- getArgs
  case args of 
      []      -> firefox2epub defaultEbookName
      ['-':_] -> usageHelp name
      ['/':_] -> usageHelp name
      [xs]    -> firefox2epub xs
      _       -> usageHelp name

usageHelp name = putStrLn $ "Usage: " ++ name ++ " [<Title Name of new e-book>]"

firefox2epub bookName = do
  arts <- wiki4e_listFirefoxURLs
  config <- wiki4e_initConfig
  putStrLn "# STAGE 1/4 - Download Articles..."
  wiki4e_fetchArticles config arts
  putStrLn "# STAGE 2/4 - Sanitize Articles..."
  wiki4e_sanitizeArticles config arts
  putStrLn "# STAGE 3/4 - Download Images..."
  imgs <- wiki4e_listArticlesImages config arts
  wiki4e_fetchImages config imgs
  putStrLn "# STAGE 4/4 - Constructing EPUB..."
  wiki4e_createEpub config bookName arts imgs
  putStrLn "Done."

wiki4e_listFirefoxURLs :: IO [URL]
wiki4e_listFirefoxURLs = do
     xs <- listAllHistoryURLs
     return $ nub $ filter isArticleURL xs
