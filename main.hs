import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
import Photochop
import Test.QuickCheck

main = do 
    argument <- getArgs 
    if ((head argument) == "-t") && (1 == (length argument)) then do
        quickCheck prop_flip
        quickCheck prob_pixelMax
        quickCheck prop_numPixels
        else do
            (file1:file2:args) <- getArgs
            if null args
                then copy file1 file2
                else do
                    copy file1 file2
                    forM args (\x -> do
                        transform file2 file2 x)
                    return ()

copy :: String -> String -> IO ()
copy file1 file2 = do 
    contents <- readFile file1
    writeFile file2 contents


transform :: String -> String -> String -> IO ()
transform file1 file2 flag = do 
    contents <- readFile file1
    length contents `seq` (writeFile file2 (aplicaFlag flag (textoParaFicheiro contents)))

