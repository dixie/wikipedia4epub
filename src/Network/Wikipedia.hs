module Network.Wikipedia (isArticleURL, getArticleTitle, fetchPrintArticle)
where
import Network.URL
import Text.Regex.Base
import Text.Regex.PCRE
import Network.HTTP
import Text.HTML.TagSoup

isArticleURL :: URL -> Bool
isArticleURL (URL (Absolute (Host (HTTP False) xs Nothing)) ph []) = (xs =~ ".*[.]wikipedia.org$") && (ph =~ "wiki/.*" )
isArticleURL _ = False

getArticleTitle :: URL -> String
getArticleTitle x | isArticleURL x = tail $ dropWhile (/='/') (url_path x)
                  | otherwise      = ""
                    
-- http://en.wikipedia.org/w/index.php?title=Computer&printable=yes
articleURL2PrintURL :: URL -> URL
articleURL2PrintURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",getArticleTitle xs),("printable","yes")]
                       | otherwise       = xs

sanitize :: String -> String
sanitize xs = let inTags = parseTags xs
                  outTags = filterScripts inTags
              in renderTags outTags

filterScripts xs = xs

fetchPrintArticle :: URL -> IO (String,String)
fetchPrintArticle xs = do
    contents <- getResponseBody =<< simpleHTTP (getRequest (exportURL (articleURL2PrintURL xs)))
    return ((getArticleTitle xs),sanitize contents)
