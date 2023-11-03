{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import qualified Data.Vector as VE
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
-- import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Directory
import System.Environment
import System.FilePath

data DirItem = DirItem {dirItemIsDir :: Bool, dirItemName :: String, dirItemFullPath :: String}

instance Show DirItem where
  show item = (if dirItemIsDir item then "d " else "- ") <> dirItemName item

data AppState = AppState {_currentList :: L.List () DirItem}

makeLenses ''AppState

rootDirFromArgs :: [String] -> String
rootDirFromArgs [] = "/home/kodus/testi"
rootDirFromArgs (arg : _) = arg

itemsList :: VE.Vector a -> L.List () a
itemsList xs = L.list () xs 3

fromPath :: FilePath -> FilePath -> IO DirItem
fromPath root filePath = do
  let fullPath' = root </> filePath
  isDir' <- doesDirectoryExist fullPath'
  return DirItem {dirItemIsDir = isDir', dirItemName = filePath, dirItemFullPath = fullPath'}

getItems :: FilePath -> IO (VE.Vector DirItem)
getItems path = do
  contentPahts <- listDirectory path
  items <- mapM (fromPath path) contentPahts
  return $ VE.fromList items

createAppState :: FilePath -> IO AppState
createAppState rootDir = do
  items <- getItems rootDir
  return AppState {_currentList = itemsList items}

renderDirItem :: Bool -> DirItem -> Widget ()
renderDirItem isSelected item = str $ selectedArrow <> show item
  where
    selectedArrow = if isSelected then "-> " else ""

renderApp :: AppState -> Widget ()
renderApp appState = L.renderList renderDirItem True (appState ^. currentList)

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

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey exitKey [] | exitKey `elem` [V.KEsc, V.KChar 'q'] -> M.halt
  ev -> T.zoom currentList $ L.handleListEvent ev
appEvent _ = return ()

theApp :: M.App AppState e ()
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
