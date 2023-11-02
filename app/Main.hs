module Main (main) where

import Brick
import Brick.Widgets.List (GenericList, list, renderList)
import Data.Sequence (Seq, fromList)
import System.Directory
import System.Environment
import System.FilePath

data Item = Item {isDir :: Bool, name :: String, fullPath :: String}

instance Show Item where
  show item = (if isDir item then "d " else "- ") ++ name item

rootDirFromArgs :: [String] -> String
rootDirFromArgs [] = "/home/kodus/testi"
rootDirFromArgs (arg : _) = arg

itemsList :: Seq a -> GenericList String Seq a
itemsList xs = list "Current dir items" xs 3

fromPath :: FilePath -> FilePath -> IO Item
fromPath root filePath = do
  let fullPath' = root </> filePath
  isDir' <- doesDirectoryExist fullPath'
  return Item {isDir = isDir', name = filePath, fullPath = fullPath'}

getItems :: FilePath -> IO (Seq Item)
getItems path = do
  contentPahts <- listDirectory path
  items <- mapM (fromPath path) contentPahts
  return $ fromList items

main :: IO ()
main = do
  args <- getArgs
  let rootDir = rootDirFromArgs args
  items <- getItems rootDir
  let genList = itemsList items
  let r = renderList (\_ e -> str (show e)) False genList
  simpleMain r
