import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Wiki4e.Commands
import Network.Wikipedia (articleURL2Title)
import System.IO 
import Network.URL
import Data.Maybe (fromJust)

-- Download only intermediate children
defaultDepth = 1

main = do
  name <- getProgName
  args <- getArgs
  case args of 
      ['-':_] -> usageHelp name
      ['/':_] -> usageHelp name
      [u]     -> subtree2epub u defaultDepth
      _       -> usageHelp name

usageHelp name = putStrLn $ "Usage: " ++ name ++ " <Start URL>"

subtree2epub cs l = do
  let u = fromJust $ importURL cs
  config <- wiki4e_initConfig
  putStrLn $ "# INFO - download depth is hardcoded value = " ++ (show defaultDepth)
  putStrLn $ "# STAGE 1/4 - Fetch starting article: " ++ (exportURL u)
  arts <- wiki4e_crawlArticlesLinks config [u] l
  putStrLn $ "# STAGE 2/4 - Fetch children articles: " ++ (show (length arts))
  wiki4e_fetchArticles config arts
  putStrLn "# STAGE 3/4 - Sanitize articles"
  wiki4e_sanitizeArticles config arts
  putStrLn "# STAGE 4/4 - Download images"
  imgs <- wiki4e_listArticlesImages config arts
  putStrLn $ "Count = " ++ (show $ length imgs)
  wiki4e_fetchImages config imgs
  putStrLn "# STAGE 5/4 - Constructing EPUB"
  wiki4e_createEpub config (articleURL2Title u) arts imgs
  putStrLn "Done."

