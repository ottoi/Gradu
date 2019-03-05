{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

import System.Environment (getArgs)
import Text.Regex.Posix
import System.Directory
import Data.Word
import Data.List as L (takeWhile,length,sort) 

import Data.Vector as V hiding (convert, (++))
import Vision.Image as I
import Vision.Image.Storage.DevIL (Autodetect (..), JPG (..), StorageImage (..), load, save)
import Vision.Primitive (Z (..), (:.) (..), Rect (..), ix2)
--import Vision.Image.Conversion
--import Data.Word

import Functions

-------------------------------------------------------------------------------


main :: IO ()
main = do
    -- In Linux /home/user/path/ /home/user/anotherPath/data.dat IMG.* 
    [inputPath, outputPath, regex] <- getArgs
  {- 
    let
      inputPath = "/home/otto/Koulu/KPZ/UudetPoltot/poltto_001/" 
      outputPath = "/home/otto/Koulu/KPZ/TESTI.dat"
      regex = "IMG_003.*"
  -}
    files <- getDirectoryContents inputPath
    let 
      regexFun :: FilePath -> Bool
      regexFun s = s =~ regex 
      
      pictures = L.sort $ Prelude.filter regexFun files

    putStrLn $ "There are " ++(show $ L.length pictures)++ " pictures"
    putStrLn $ show  (Prelude.map (\a -> a ++"  ") $ L.sort pictures)

    Prelude.mapM_ (doDetection inputPath outputPath) pictures


doDetection :: FilePath -> FilePath -> String -> IO ()
doDetection inputPath outputPath input = do 
    let path = inputPath ++ input :: FilePath
    io <- load Autodetect path --input

    case io of
        Left err           -> do
            putStrLn $ "Unable to load the file " ++input++ ":"
            print err
        Right (grey :: Grey) -> do
            let 
            -- Gets the size of the image.
                Z :. h :. w = shape grey

                 -- Here happens the magic.
                pixels = fmap (fmap (h-)) $ 
                    frontOfTheInterface 2 900 1024 grey 
                    --brightestPixels (200) grey
            appendFile outputPath $ showData pixels
            putStrLn input


showData :: Vector (Maybe Int) -> String
showData v = sFun "" 0
    where
        sFun s i = if l > i 
          then
            case (v V.! i) of
                Nothing -> sFun (s ++ "  NaN") (i+1) 
                Just a -> sFun (s ++"  "++ show a) (i+1)
          else  
                s ++ "\n"
        l = V.length v










