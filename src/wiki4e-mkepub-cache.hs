import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Data.Maybe (catMaybes)
import Control.Monad (liftM)
import System.IO 
import Network.URL
import Wiki4e.Commands
import Network.Wikipedia (isArticleURL)

defaultEbookName = "Wikipedia_Articles_From_Cache"

main = do
  name <- getProgName
  args <- getArgs
  case args of 
      []      -> cache2epub defaultEbookName
      ['-':_] -> usageHelp name
      ['/':_] -> usageHelp name
      [xs]    -> cache2epub xs
      _       -> usageHelp name

usageHelp name = putStrLn $ "Usage: " ++ name ++ " [<Title Name of new e-book>]"

cache2epub bookName = do
  hSetBinaryMode stdout True
  config <- wiki4e_initConfig
  arts <- wiki4e_listCacheURLs config
  putStrLn "# STAGE 1/5 - Verify Articles..."
  wiki4e_fetchArticles config arts
  putStrLn "# STAGE 2/5 - Sanitize Articles..."
  wiki4e_sanitizeArticles config arts
  putStrLn "# STAGE 3/5 - Inspect Articles for Images..."
  imgs <- wiki4e_listArticlesImages config arts
  putStrLn "# STAGE 4/5 - Download Images..."
  wiki4e_fetchImages config imgs
  putStrLn "# STAGE 5/5 - Constructing EPUB..."
  wiki4e_createEpub config bookName arts imgs
  putStrLn "Done."

-- | It is expected that all articles are from english wikipedia
wiki4e_listCacheURLs :: Wiki4eConfig -> IO [URL]
wiki4e_listCacheURLs config = do
  files <- liftM (filter (\(c:_) -> c /= '.')) $ getDirectoryContents (w4confDirFetch config)
  return $ catMaybes $ map (importURL) (map (\x -> "http://en.wikipedia.org/wiki/"++x) files)
