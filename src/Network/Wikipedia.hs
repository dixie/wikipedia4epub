module Network.Wikipedia ( isArticleURL
                         , WikiArticle(..)
                         , articleURL2Title
                         , getArticleLinks
                         , getArticleLinksAbs
                         , getArticleImages
                         , sanitizeArticle
                         , sanitizeFileName
                         --, fetchPrintArticle
                         --, fetchRawArticle
                         , fetchArticle)
where
import Network.URL
import Text.Regex.Posix
import Network.HTTP
import Text.HTML.TagSoup
import System.FilePath
import Data.List (nub)
import Data.Maybe (mapMaybe, fromJust)
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Codec.Compression.GZip as GZip

data WikiArticle = WikiArticleHTML { waTitle :: String, waContent :: String } 
                 | WikiArticleSRC  { waTitle :: String, waContent :: String } deriving (Show, Ord, Eq)

sanitizeFileName :: FilePath -> FilePath
sanitizeFileName cs = map removeNonAscii $ makeValid cs
  where
    removeNonAscii z | isAscii z = z
                     | otherwise = '_'

isArticleURL :: URL -> Bool
isArticleURL (URL (Absolute (Host (HTTP False) xs Nothing)) ph []) = (xs =~ ".*[.]wikipedia.org") && (ph =~ "wiki/.*")
isArticleURL _ = False

articleURL2Title :: URL -> String
articleURL2Title x = sanitizeFileName (takeFileName (url_path x))

articleRelURL2Title :: String -> String 
articleRelURL2Title ('#':xs) = '#':xs
articleRelURL2Title x = case importURL ("http://en.wikipedia.org/"++x) of 
                          Nothing -> error $ "Invalid article relative path: "++x
                          Just  u -> articleURL2Title u

isArticleImgURL :: String -> Bool
isArticleImgURL cs = cs =~ "^http://upload.wikimedia.org/.*"

getArticleImages :: WikiArticle -> [URL]
getArticleImages x = let inTags = parseTags (waContent x)
                         imgTags = filter (isTagOpenName "img") inTags
                         imgSrcs = filter (isArticleImgURL) (map (fromAttrib "src") imgTags)
                     in imgSrcs `seq` mapMaybe importURL imgSrcs

getArticleLinks :: WikiArticle -> [URL]
getArticleLinks xs = let inTags = parseTags (waContent xs)
                         aTags = filter (isTagOpenName "a") inTags
                         hrefs = map (fromAttrib "href") aTags 
                     in mapMaybe importURL $ nub $ filter (=~ "^/wiki/[^:/]+$") hrefs

getArticleLinksAbs :: String -> WikiArticle -> [URL]
getArticleLinksAbs domain xs = map toAbsURL ys
  where
    ys = getArticleLinks xs
    toAbsURL (URL _ path params) = (URL (Absolute (Host (HTTP False) domain Nothing)) path params)
                    
-- http://en.wikipedia.org/w/index.php?title=Computer&printable=yes
{-
articleURL2PrintURL :: URL -> URL
articleURL2PrintURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",articleURL2Title xs),("printable","yes")]
                       | otherwise       = xs

-- http://en.wikipedia.org/wiki/index.php?title=Psychology&action=raw
articleURL2RawURL :: URL -> URL
articleURL2RawURL xs | isArticleURL xs = URL (url_type xs) "w/index.php" [("title",articleURL2Title xs),("action","raw")]
                     | otherwise       = xs
-}

sanitizeArticle :: [String] -> WikiArticle -> WikiArticle
sanitizeArticle alnk xs = let inTags = parseTags (waContent xs)
                              outTags = processTags alnk $ filterAllTags tags4Filter inTags
                              tags4Filter = ["link", "script", "sup" ]
                          in WikiArticleHTML (waTitle xs) (renderTags outTags)

processTags alnk xs = procHrefTags alnk $ procImgTags $ map processAttrs xs
   where
     processAttrs (TagOpen  "div" _) = TagText ""
     processAttrs (TagClose "div") = TagText ""
     processAttrs t@(TagOpen s ys) | null s        = t
                                   | head s == '!' = t
                                   | otherwise     = TagOpen s (removeStyleAttr $ removeEmptyAttr ys)
     processAttrs t = t
     removeEmptyAttr = filter (not . null . snd)
     removeStyleAttr = filter (\x -> (fst x) `notElem` ["style", "class"])


filterAllTags tgs xs = filter (not . isTagComment) $ foldr (filterTags) xs tgs 

isTagComment (TagComment _) = True
isTagComment _              = False

filterTags _ [] = []
filterTags tn (x:xs) | isTagOpenName tn x  = filterTags tn $ dropWhile (not . isTagCloseName tn) xs
                     | isTagCloseName tn x = filterTags tn xs
                     | otherwise           = x:filterTags tn xs


procHrefTags _ [] = []
procHrefTags alnk (x:xs) | isTagOpenName "a" x = let relP   = fromAttrib "href" x
                                                     title   = articleRelURL2Title relP
                                                     imgOk  = (TagOpen "a" [("href",title)]):(procHrefTags alnk xs)
                                                     isInBook = elem (title) alnk || ((length title) > 0 && (head title == '#'))
                                                     imgNok = let pre  = takeWhile (not . isTagCloseName "a") xs
                                                                  post = tail $ dropWhile (not . isTagCloseName "a") xs
                                                              in pre ++ (procHrefTags alnk post)
                                                 in if isInBook then imgOk else imgNok
                         | otherwise           = x:(procHrefTags alnk xs)

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
      rsp <- Network.HTTP.simpleHTTP (insertHeader HdrAcceptEncoding "identity" $ getRequest (exportURL xs))
      -- rsp <- Network.HTTP.simpleHTTP (getRequest (exportURL xs))
      contents <- (getResponseBody rsp)
      let header = either (\_ -> []) rspHeaders rsp
      let fincts = case lookupHeader HdrContentEncoding header of 
                       Nothing -> contents
                       Just mm -> if mm == "gzip" then BC.unpack (GZip.decompress (BC.pack contents)) else contents
      return (WikiArticleHTML (articleURL2Title xs) fincts)
