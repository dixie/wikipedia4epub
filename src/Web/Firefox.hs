module Web.Firefox (listPlacesFiles, listHistoryURLs, listAllHistoryURLs) 
where
import Database.HDBC (disconnect, fromSql, quickQuery')
import Database.HDBC.Sqlite3 (connectSqlite3, setBusyTimeout)
import Network.URL (importURL, URL)
import Data.List (isSuffixOf)
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory, getTemporaryDirectory, copyFile, removeFile)
import System.FilePath ((</>))

firefoxDirs :: [String]
firefoxDirs = [ ".mozilla", ".mozilla-firefox", ".firefox" ]

placesFile :: String
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
   tmpDir <- getTemporaryDirectory
   let tmpName = tmpDir </> "wiki4e_places.sqlite"
   copyFile name tmpName
   conn <- connectSqlite3 tmpName
   setBusyTimeout conn (1000*timeoutFox)
   xs <- quickQuery' conn "select url from moz_places order by last_visit_date desc" []
   let ys = map (fromSql . head) xs :: [String]
   removeFile tmpName
   return $ mapMaybe importURL ys

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  e <- doesDirectoryExist topdir
  if e then do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = topdir </> name
      e2 <- doesDirectoryExist path
      if e2
        then getRecursiveContents path
        else return [path]
    return (concat paths)
    else return []
