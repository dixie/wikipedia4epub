import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Wiki4e.Commands
import System.IO 
import Network.URL

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
  xs <- wiki4e_listFirefoxURLs
  (tmpDirFetch,tmpDirSanitize,tmpDirImgs) <- wiki4e_initConfig
  putStrLn "# STAGE 1/4 - Download Articles..."
  wiki4e_fetchArticles tmpDirFetch xs
  putStrLn "# STAGE 2/4 - Sanitize Articles..."
  wiki4e_sanitizeArticles tmpDirFetch tmpDirSanitize
  putStrLn "# STAGE 3/4 - Download Images..."
  imgs <- wiki4e_listArticlesImages tmpDirFetch
  wiki4e_fetchImages tmpDirImgs imgs
  putStrLn "# STAGE 4/4 - Constructing EPUB..."
  wiki4e_createEpub bookName tmpDirSanitize tmpDirImgs
  putStrLn "Done."
