module Wiki4e.Commands.EPUB (wiki4e_createEpub) where
import Network.URL
import Data.List (foldl')
import Codec.EBook
import qualified Data.ByteString.Lazy as STRL
import Wiki4e.Commands.Common
import Wiki4e.Commands.Images
import Wiki4e.Commands.Articles

-- | Create epub in current directory with bookName 
-- | With all articles in directory srcDir and images at
-- | imgDir
wiki4e_createEpub :: Wiki4eConfig -> String -> [URL] -> [URL] -> IO ()
wiki4e_createEpub config bookName artURLs imgURLs = do
     let book = emptyBook { 
                  bookID = "http://localhost/"++bookName,
                  bookAuthor = "wiki4e-firefox-epub",
                  bookTitle = bookName
                }
     let xs = wiki4e_getArticleSanFiles config artURLs
     let ys = wiki4e_getImgFiles config imgURLs
     itemsA <- mapM (\(i,x) -> loadArticleFile i "wiki" x) $ zip [1..] xs
     itemsI <- mapM (\(i,x) -> loadImgFile i "wiki/img" x) $ zip [(length xs + 1)..] ys
     let bookFull = foldl' addItem2Book book (itemsA ++ itemsI)
     let epubFName = bookName++".epub"
     outdata <- book2Bin' bookFull
     STRL.writeFile epubFName  outdata
     putStrLn $ epubFName ++ " constructed."
