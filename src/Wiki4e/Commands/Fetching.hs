module Wiki4e.Commands.Fetching where
import System.Directory
import Network.URL
import System.IO

-- Fetching state where first number is total and second is current item.
type FetchingState = (Int,Int)
type FetchingMethod = (URL -> IO String)

fs2str :: FetchingState -> String
fs2str (total,current) = "["++(show current)++"/"++(show total)++"] " 

wiki4e_fetch :: FilePath -> FetchingMethod -> FetchingState -> URL -> IO ()
wiki4e_fetch outf fm fs x = do
  e <- doesFileExist outf
  if not e then do
    putStrLn $ (fs2str fs) ++ "Fetching : " ++ exportURL x
    c <- fm x
    withBinaryFile outf WriteMode (flip hPutStr c)
    else putStrLn $ (fs2str fs) ++ "Already cached. Skipping download. " ++ outf
