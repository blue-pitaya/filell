module Main (main) where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.List as L
import Control.Monad (void)
import Data.Sequence (Seq, fromList)
import qualified Graphics.Vty as V
import System.Directory
import System.Environment
import System.FilePath

data DirItem = Item {dirItemIsDir :: Bool, dirItemName :: String, dirItemFullPath :: String}

instance Show DirItem where
  show item = (if dirItemIsDir item then "d " else "- ") ++ dirItemName item

data AppState = AppState {currentItems :: Seq DirItem, selectedIdx :: Int}

rootDirFromArgs :: [String] -> String
rootDirFromArgs [] = "/home/kodus/testi"
rootDirFromArgs (arg : _) = arg

itemsList :: Seq a -> GenericList String Seq a
itemsList xs = list "Current dir items" xs 3

fromPath :: FilePath -> FilePath -> IO DirItem
fromPath root filePath = do
  let fullPath' = root </> filePath
  isDir' <- doesDirectoryExist fullPath'
  return Item {dirItemIsDir = isDir', dirItemName = filePath, dirItemFullPath = fullPath'}

getItems :: FilePath -> IO (Seq DirItem)
getItems path = do
  contentPahts <- listDirectory path
  items <- mapM (fromPath path) contentPahts
  return $ fromList items

createAppState :: FilePath -> IO AppState
createAppState rootDir = do
  items <- getItems rootDir
  return AppState {currentItems = items, selectedIdx = 0}

renderDirItem :: Bool -> DirItem -> Widget String
renderDirItem isSelected item = str $ selectedArrow ++ show item
  where
    selectedArrow = if isSelected then "-> " else ""

renderApp :: AppState -> Widget String
renderApp appState = renderList renderDirItem True (itemsList $ currentItems appState)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.white `on` V.blue),
      (customAttr, fg V.red)
    ]

appEvent :: T.BrickEvent String e -> T.EventM String AppState ()
appEvent _ = M.halt

theApp :: M.App AppState e String
theApp =
  M.App
    { M.appDraw = \state -> [renderApp state],
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = do
  args <- getArgs
  let rootDir = rootDirFromArgs args
  initialState <- createAppState rootDir
  void $ M.defaultMain theApp initialState
