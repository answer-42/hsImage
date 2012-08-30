module ImagePaths where


import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), takeExtension)

import Control.Monad (forM)


import Data.List (isSuffixOf)

imageExtensions = [".jpg",".png"]

-- Traverse Files
getRecursiveFiles :: String -> IO [String]
getRecursiveFiles topdir = do
  names <- getDirectoryContents topdir
  let properFiles = filter (`notElem` [".", ".."]) names
  paths <- forM properFiles $ \name -> do
                             let path = topdir </> name
                             isDirectory <- doesDirectoryExist path
                             if isDirectory
                             then getRecursiveFiles path
                             else return [path]
  return (concat paths)

{-- WARNING: getImages builds a list of all files recursivley, and then it 
             retrieves the images. A better algorithm is needed if you have
             a lot of images.
--}
getImages :: String -> IO [String]
getImages toplevel = do
  files <- getRecursiveFiles toplevel 
  return (filter isImage files)
    where
      isImage :: String -> Bool
      isImage s = any (`isSuffixOf` s) imageExtensions
