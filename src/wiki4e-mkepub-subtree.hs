import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Wiki4e.Commands
import System.IO 
import Network.URL
import Data.Maybe (fromJust)

defaultDepth = 2

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
  (tmpDirFetch,tmpDirSanitize,tmpDirImgs) <- wiki4e_initConfig
  putStrLn $ "# STAGE 1/4 - Crawl and fetch articles (max depth = "++(show l)++")"
  xs <- wiki4e_crawlArticlesLinks tmpDirFetch [u] l
  putStrLn $ " Found "++(show (length xs))++" Links"
  wiki4e_fetchArticles tmpDirFetch xs
  putStrLn $ " Fetched "++(show (length xs))++" Links"
  putStrLn "# STAGE 2/4 - Sanitize articles..."
  wiki4e_sanitizeArticles tmpDirFetch tmpDirSanitize
  putStrLn "# STAGE 3/4 - Download images..."
  imgs <- wiki4e_listArticlesImages tmpDirFetch
  wiki4e_fetchImages tmpDirImgs imgs
  putStrLn "# STAGE 4/4 - Constructing EPUB..."
  -- wiki4e_createEpub bookName tmpDirSanitize tmpDirImgs
  putStrLn "Done."

