import Text.HTML.TagSoup
import System.Environment
import Wikipedia4epub.Commands
import System.IO 

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
  wiki4e_fetchArticles "wiki" xs
  putStrLn "Done."
