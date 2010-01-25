module Network.Wikipedia ( isArticleURL
                         , WikiArticle(..)
                         , getArticleTitle
                         , getArticleLinks
                         , fetchPrintArticle
                         , fetchRawArticle
                         , fetchArticle)
where
import Network.URL
import Text.Regex.Base
import Text.Regex.PCRE
import Network.HTTP
import Text.HTML.TagSoup
import Data.List (nub)
import Data.Maybe (catMaybes)

data WikiArticle = WikiArticleHTML { waTitle :: String, waContent :: String } 
                 | WikiArticleSRC  { waTitle :: String, waContent :: String } deriving (Show, Ord, Eq)

isArticleURL :: URL -> Bool
isArticleURL (URL (Absolute (Host (HTTP False) xs Nothing)) ph []) = (xs =~ ".*[.]wikipedia.org$") && (ph =~ "wiki/.*" )
isArticleURL _ = False

getArticleTitle :: URL -> String
getArticleTitle x | isArticleURL x = tail $ dropWhile (/='/') (url_path x)
                  | otherwise      = ""

getArticleLinks :: WikiArticle -> [URL]
getArticleLinks xs = let inTags = parseTags (waContent xs)
                         aTags = filter (isTagOpenName "a") inTags
                         hrefs = map (fromAttrib "href") aTags 
                     in catMaybes $ map (importURL) $ nub $ filter (\x -> x =~ "^/wiki/[^:/]+$") hrefs
                    
-- http://en.wikipedia.org/w/index.php?title=Computer&printable=yes
articleURL2PrintURL :: URL -> URL
articleURL2PrintURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",getArticleTitle xs),("printable","yes")]
                       | otherwise       = xs

-- http://en.wikipedia.org/wiki/index.php?title=Psychology&action=raw
articleURL2RawURL :: URL -> URL
articleURL2RawURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",getArticleTitle xs),("action","raw")]
                     | otherwise       = xs

sanitizeArticle :: WikiArticle -> WikiArticle
sanitizeArticle xs = let inTags = parseTags (waContent xs)
                         outTags = filterTags "img" $ filterTags "div" $ filterTags "link" $ filterTags "script" inTags
                     in WikiArticleHTML (waTitle xs) (renderTags outTags)

filterTags tn [] = []
filterTags tn (x:xs) | isTagOpenName tn x  = filterTags tn $ dropWhile (not . isTagCloseName tn) xs
                     | isTagCloseName tn x = filterTags tn xs
                     | otherwise           = x:filterTags tn xs

fetchPrintArticle :: URL -> IO WikiArticle
fetchPrintArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL (articleURL2PrintURL xs)))
    return (WikiArticleHTML (getArticleTitle xs) contents)

fetchRawArticle :: URL -> IO WikiArticle
fetchRawArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL (articleURL2RawURL xs)))
    return (WikiArticleSRC (getArticleTitle xs) contents)

fetchArticle :: URL -> IO WikiArticle
fetchArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL xs))
    return (WikiArticleHTML (getArticleTitle xs) contents)
