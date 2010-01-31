module Network.Wikipedia ( isArticleURL
                         , WikiArticle(..)
                         , articleURL2Title
                         , getArticleLinks
                         , sanitizeArticle
                         --, fetchPrintArticle
                         --, fetchRawArticle
                         , fetchArticle)
where
import Network.URL
import Network.URI
import Text.Regex.Posix
import Text.Regex.Base
import Network.HTTP
import Text.HTML.TagSoup
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe, fromJust)

data WikiArticle = WikiArticleHTML { waTitle :: String, waContent :: String } 
                 | WikiArticleSRC  { waTitle :: String, waContent :: String } deriving (Show, Ord, Eq)

isArticleURL :: URL -> Bool
isArticleURL (URL (Absolute (Host (HTTP False) xs Nothing)) ph []) = (xs =~ ".*en[.]wikipedia.org$") && (ph =~ "wiki/[^:/]+$" )
isArticleURL _ = False

articleURL2Title :: URL -> String
articleURL2Title x | isArticleURL x = filter (/= '%') $ urlEncode $ tail $ dropWhile (/='/') (url_path x)
                   | otherwise      = ""

getArticleLinks :: WikiArticle -> [URL]
getArticleLinks xs = let inTags = parseTags (waContent xs)
                         aTags = filter (isTagOpenName "a") inTags
                         hrefs = map (fromAttrib "href") aTags 
                     in mapMaybe importURL $ nub $ filter (=~ "^/wiki/[^:/]+$") hrefs
                    
-- http://en.wikipedia.org/w/index.php?title=Computer&printable=yes
articleURL2PrintURL :: URL -> URL
articleURL2PrintURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",articleURL2Title xs),("printable","yes")]
                       | otherwise       = xs

-- http://en.wikipedia.org/wiki/index.php?title=Psychology&action=raw
articleURL2RawURL :: URL -> URL
articleURL2RawURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",articleURL2Title xs),("action","raw")]
                     | otherwise       = xs

sanitizeArticle :: WikiArticle -> WikiArticle
sanitizeArticle xs = let inTags = parseTags (waContent xs)
                         outTags = processTags $ filterTags "img" $ filterTags "div" $ filterTags "link" $ filterTags "script" inTags
                     in WikiArticleHTML (waTitle xs) (renderTags outTags)

processTags xs = map removeEmptyAttr xs
   where
     removeEmptyAttr t@(TagOpen s xs) | null s        = t
                                      | head s == '!' = t
                                      | otherwise     = TagOpen s (filter (not . null . snd) xs) 
     removeEmptyAttr t = t

filterTags tn [] = []
filterTags tn (x:xs) | isTagOpenName tn x  = filterTags tn $ dropWhile (not . isTagCloseName tn) xs
                     | isTagCloseName tn x = filterTags tn xs
                     | otherwise           = x:filterTags tn xs
{-
fetchPrintArticle :: URL -> IO WikiArticle
fetchPrintArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL (articleURL2PrintURL xs)))
    return (WikiArticleHTML (articleURL2Title xs) contents)

fetchRawArticle :: URL -> IO WikiArticle
fetchRawArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL (articleURL2RawURL xs)))
    return (WikiArticleSRC (articleURL2Title xs) contents)
-}

fetchArticle :: URL -> IO WikiArticle
fetchArticle xs = do
      rsp <- Network.HTTP.simpleHTTP (getRequest (exportURL xs))
      contents <- (getResponseBody rsp)
      return (WikiArticleHTML (articleURL2Title xs) contents)
