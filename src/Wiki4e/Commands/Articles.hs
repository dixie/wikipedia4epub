module Wiki4e.Commands.Articles ( wiki4e_sanitizeArticle
                                , wiki4e_sanitizeArticles
                                , wiki4e_fetchArticles
                                , wiki4e_getArticleFiles
                                , wiki4e_getArticleSanFiles
                                , wiki4e_crawlArticlesLinks
                                , wiki4e_listArticleImages
                                , wiki4e_listArticlesImages
                                , wiki4e_readArticle
                                , loadArticleFile
                                , articleURL2File
                                , articleURL2SanFile)
where
import System.FilePath
import Network.URL
import Data.List (nub)
import System.IO
import Codec.EBook
import qualified Data.ByteString as STR
import Network.Wikipedia
import Wiki4e.Commands.Common
import Wiki4e.Commands.Fetching

wiki4e_sanitizeArticle :: [String] -> FilePath -> FilePath -> IO ()
wiki4e_sanitizeArticle alnk inf ouf = withFile inf ReadMode (\hi -> do
          hSetEncoding hi utf8
          withFile ouf WriteMode (\ho -> do 
                  hSetEncoding ho utf8
                  c <- hGetContents hi
                  let a = sanitizeArticle alnk $ WikiArticleHTML "" c
                  hPutStr ho $ waContent a
               )
         )

wiki4e_fetchArticles :: Wiki4eConfig -> [URL] -> IO ()
wiki4e_fetchArticles config xs = mapM_ (\(st,x) -> wiki4e_fetch (nm x) fm st x) $ zip fsls xs
    where
      fsls = [ (total,i) | i <- [1..total] ]
      total = length xs
      nm x = articleURL2File config x
      fm x = do
        (WikiArticleHTML _ c) <- fetchArticle x
        return c

wiki4e_sanitizeArticles :: Wiki4eConfig -> [URL] -> IO ()
wiki4e_sanitizeArticles config arts = do
  mapM_ (\x -> wiki4e_sanitizeArticle alnk x (outf x)) xs
    where
      xs = wiki4e_getArticleFiles config arts
      alnk = map (articleURL2Title) arts
      outf x = (w4confDirSanitized config) </> (takeFileName x)

wiki4e_listArticleImages :: FilePath -> IO [URL]
wiki4e_listArticleImages x = do
          c <- readFileUTF8 x
          return $ nub $ getArticleImages (WikiArticleHTML "" c)

-- | Method expects already sanitized articles
wiki4e_listArticlesImages :: Wiki4eConfig -> [URL] -> IO [URL]
wiki4e_listArticlesImages config urls = do 
     let files = wiki4e_getArticleFiles config urls
     images <- mapM (wiki4e_listArticleImages) files
     return $ nub $ concat images

wiki4e_crawlArticlesLinks :: Wiki4eConfig -> [URL] -> Int -> IO [URL]
wiki4e_crawlArticlesLinks _ _  0 = return []
wiki4e_crawlArticlesLinks _ [] _ = return []
wiki4e_crawlArticlesLinks config us (l + 1) = do
  wiki4e_fetchArticles config us
  let fs = map (outd </>) $ map (articleURL2Title) us
  as <- mapM (wiki4e_readArticle) fs
  let xs = nub $ concat $ map getArticleLinksAbs as
  ys <- wiki4e_crawlArticlesLinks config xs l
  return (nub $ us++xs++ys)
    where
      outd = w4confDirFetch config

wiki4e_getArticleFiles :: Wiki4eConfig -> [URL] -> [FilePath]
wiki4e_getArticleFiles config xs = map (articleURL2File config) xs

wiki4e_getArticleSanFiles :: Wiki4eConfig -> [URL] -> [FilePath]
wiki4e_getArticleSanFiles config xs = map (articleURL2SanFile config) xs

articleURL2File :: Wiki4eConfig -> URL -> FilePath
articleURL2File config x = (w4confDirFetch config) </> (articleURL2Title x)

articleURL2SanFile :: Wiki4eConfig -> URL -> FilePath
articleURL2SanFile config x = (w4confDirSanitized config) </> (articleURL2Title x)

loadArticleFile :: Int -> FilePath -> FilePath -> IO BookItem
loadArticleFile i bookDir fname = do
   cs <- STR.readFile fname
   return (BookItem aid bfile (toLazy cs) opsMediatype (Just (ChapterMetadata name))) 
   where
      aid = show i
      bfile = bookDir </> name
      name = takeFileName $ normalise fname

wiki4e_readArticle :: FilePath -> IO WikiArticle
wiki4e_readArticle inf = do
  c <- readFileUTF8 inf
  return (WikiArticleHTML "" c)
