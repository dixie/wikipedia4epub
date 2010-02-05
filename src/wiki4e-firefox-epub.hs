import Text.HTML.TagSoup
import System.Environment
import System.Directory
import System.FilePath
import Wikipedia4epub.Commands
import System.IO 
import Network.URL

main = do
  name <- getProgName
  args <- getArgs
  case args of 
      []      -> firefox2epub "Wikipedia_Articles_From_Firefox"
      ['-':_] -> usageHelp name
      ['/':_] -> usageHelp name
      [xs]    -> firefox2epub xs
      _       -> usageHelp name

usageHelp name = putStrLn $ "Usage: " ++ name ++ " [<Title Name of new e-book>]"

firefox2epub bookName = do
  xs <- wiki4e_listFirefoxURLs
  tmpDir <- getTemporaryDirectory
  let tmpDirFetch    = tmpDir </> "wiki4e_fetch"     
  let tmpDirSanitize = tmpDir </> "wiki4e_sanitize"
  let tmpDirImgs     = tmpDir </> "wiki4e_images"
  createDirectoryIfMissing True tmpDirFetch
  createDirectoryIfMissing True tmpDirSanitize
  createDirectoryIfMissing True tmpDirImgs
  putStrLn $ "Fetching "++show (length xs)++" Articles..."
  wiki4e_fetchArticles tmpDirFetch xs
  putStrLn "Sanitize Articles..."
  wiki4e_sanitizeArticles tmpDirFetch tmpDirSanitize
  putStrLn "Download Images..."
  imgs <- wiki4e_listArticlesImages tmpDirFetch
  wiki4e_fetchImages tmpDirImgs imgs
  putStrLn "Constructing EPUB..."
  wiki4e_createEpub bookName tmpDirSanitize tmpDirImgs
  putStrLn "Done."
