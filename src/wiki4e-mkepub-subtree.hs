import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Wiki4e.Commands
import Network.Wikipedia (articleURL2Title)
import System.IO 
import Network.URL
import Data.Maybe (fromJust)

defaultDepth = 1

main = do
  name <- getProgName
  args <- getArgs
  case args of 
      ['-':_] -> usageHelp name
      ['/':_] -> usageHelp name
      [u]     -> subtree2epub u defaultDepth
      [u,l]   -> subtree2epub u (read l)
      _       -> usageHelp name

usageHelp name = putStrLn $ "Usage: " ++ name ++ " <Start URL> [<Max Depth>]"

subtree2epub cs l = do
  let u = fromJust $ importURL cs
  config <- wiki4e_initConfig
  putStrLn $ "# STAGE 1/4 - Crawl and fetch articles (max depth = "++(show l)++")"
  arts <- wiki4e_crawlArticlesLinks config [u] l
  putStrLn $ " Found "++(show (length arts))++" Links"
  wiki4e_fetchArticles config arts
  putStrLn $ " Fetched "++(show (length arts))++" Links"
  putStrLn "# STAGE 2/4 - Sanitize articles..."
  wiki4e_sanitizeArticles config arts
  putStrLn "# STAGE 3/4 - Download images..."
  imgs <- wiki4e_listArticlesImages config arts
  wiki4e_fetchImages config imgs
  putStrLn "# STAGE 4/4 - Constructing EPUB..."
  wiki4e_createEpub config (articleURL2Title u) arts imgs
  putStrLn "Done."

