import Database.HDBC
import Database.HDBC.Sqlite3
import Text.Regex.Base
import Text.Regex.PCRE
import Text.HTML.TagSoup
import Network.HTTP
import System.Environment

listFirefoxPlaces :: IO [FilePath]
listFirefoxPlaces = do
  return ["places.sqlite"]

dumpHistory :: IO [String]
dumpHistory = do
  conn <- connectSqlite3 "data/places.sqlite"
  xs <- quickQuery' conn "select url from moz_places" []
  let ys = map (fromSql . head) xs :: [String]
  disconnect conn
  return ys

dumpWikipediaItems = do
  ax <- dumpHistory
  return $ filter (isArticle) ax

isArticle :: String -> Bool
isArticle x = x =~ "wikipedia.org/wiki/"

getArticlePrintURL :: String -> String
getArticlePrintURL normURL = ""

getArticleContent url = do
    contents <- getResponseBody =<< simpleHTTP (getRequest url)
    return contents

-- http://en.wikipedia.org/w/index.php?title=Dog&printable=yes

printWikimediaItems = do
  xs <- dumpWikipediaItems 
  mapM_ dumpWikipediaItem xs
  putStrLn "Done"

dumpWikipediaItem x = do
  putStrLn x
  contents <- getArticleContent x
  print contents

main = do
   printWikimediaItems
