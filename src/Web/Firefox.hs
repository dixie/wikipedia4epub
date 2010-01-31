module Web.Firefox (listPlacesFiles, listHistoryURLs, listAllHistoryURLs) 
where
import Database.HDBC (disconnect, fromSql, quickQuery')
import Database.HDBC.Sqlite3 (connectSqlite3, setBusyTimeout)
import Network.URL (importURL, URL)
import Data.List (isSuffixOf)
import Control.Monad (forM)
import Data.Maybe (catMaybes, mapMaybe)
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.FilePath ((</>))

firefoxDirs = [ ".mozilla" ]
placesFile = "places.sqlite"
timeoutFox = 5

listPlacesFiles :: IO [FilePath]
listPlacesFiles = do
   homeDir <- getHomeDirectory
   xs <- mapM (getRecursiveContents . (homeDir </>)) firefoxDirs
   return $ filter (placesFile `isSuffixOf`) (concat xs)

listAllHistoryURLs :: IO [URL]
listAllHistoryURLs = do
   putStrLn $ "Please close your Firefox if you see this message longer than "++show timeoutFox++" seconds..."
   places <- listPlacesFiles 
   xs <- mapM listHistoryURLs places
   return $ concat xs

listHistoryURLs :: FilePath -> IO [URL]
listHistoryURLs name = do
   putStrLn $ "Going to connect on Firefox SQLite DB: " ++ name
   conn <- connectSqlite3 name
   setBusyTimeout conn (1000*timeoutFox)
   xs <- quickQuery' conn "select url from moz_places order by last_visit_date desc" []
   let ys = map (fromSql . head) xs :: [String]
   return $ mapMaybe importURL ys

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
