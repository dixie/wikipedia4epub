module Network.Wikipedia ( isArticleURL
                         , WikiArticle(..)
                         , articleURL2Title
                         , getArticleLinks
                         , getArticleImages
                         , sanitizeArticle
                         , sanitizeFileName
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
import System.FilePath
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe, fromJust)

data WikiArticle = WikiArticleHTML { waTitle :: String, waContent :: String } 
                 | WikiArticleSRC  { waTitle :: String, waContent :: String } deriving (Show, Ord, Eq)


sanitizeFileName :: FilePath -> FilePath
sanitizeFileName cs = map (unPercent) $ urlEncode cs
    where
      unPercent c = if (c == '%') then 'X' else c

isArticleURL :: URL -> Bool
isArticleURL (URL (Absolute (Host (HTTP False) xs Nothing)) ph []) = (xs =~ ".*en[.]wikipedia.org$") && (ph =~ "wiki/[^:/]+$" )
isArticleURL _ = False

articleURL2Title :: URL -> String
articleURL2Title x | isArticleURL x = sanitizeFileName (takeFileName (url_path x))
                   | otherwise      = ""

isArticleImgURL :: String -> Bool
isArticleImgURL cs = cs =~ "^http://upload.wikimedia.org/.*"

getArticleImages :: WikiArticle -> [URL]
getArticleImages x = let inTags = parseTags (waContent x)
                         imgTags = filter (isTagOpenName "img") inTags
                         imgSrcs = filter (isArticleImgURL) $ map (fromAttrib "src") imgTags 
                     in mapMaybe importURL imgSrcs

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
                         outTags = procImgTags $ processTags $ filterAllTags tags4Filter inTags
                         tags4Filter = ["link", "script", "sup" ]
                     in WikiArticleHTML (waTitle xs) (renderTags outTags)

processTags xs = map processAttrs xs
   where
     processAttrs t@(TagOpen "a" _)  = TagText ""
     processAttrs t@(TagClose "a")   = TagText ""
     processAttrs t@(TagOpen  "div" _) = TagText ""
     processAttrs t@(TagClose "div") = TagText ""
     processAttrs t@(TagOpen s xs) | null s        = t
                                   | head s == '!' = t
                                   | otherwise     = TagOpen s (removeStyleAttr $ removeEmptyAttr xs)
     processAttrs t = t
     removeEmptyAttr xs = filter (not . null . snd) xs 
     removeStyleAttr xs = filter (\x -> (fst x) `notElem` ["style", "id", "class"]) xs


filterAllTags tgs xs = filter (not . isTagComment) $ foldr (filterTags) xs tgs 

isTagComment (TagComment _) = True
isTagComment _              = False

filterTags tn [] = []
filterTags tn (x:xs) | isTagOpenName tn x  = filterTags tn $ dropWhile (not . isTagCloseName tn) xs
                     | isTagCloseName tn x = filterTags tn xs
                     | otherwise           = x:filterTags tn xs

procImgTags [] = []
procImgTags (x:xs) | isTagOpenName "img" x = let absP   = fromAttrib "src" x
                                                 relP   = "img" </> (sanitizeFileName $ takeFileName $ url_path $ fromJust $ importURL absP)
                                                 imgOk  = TagOpen "img" [("src",relP)]
                                                 imgNok = tail $ dropWhile (not . isTagCloseName "img") xs
                                             in if isArticleImgURL absP then imgOk:(procImgTags xs) else procImgTags imgNok
                   | otherwise             = x:procImgTags xs

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
