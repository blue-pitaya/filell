{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Monad (void)
-- import Lens.Micro.Mtl

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Vector as VE
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH
import System.Directory
import System.Environment
import System.FilePath

data WidgetId = ParentDirListKind | CurrentDirListKind | ChildDirListId deriving (Eq, Ord, Show)

data DirItem = DirItem {dirItemIsDir :: Bool, dirItemName :: String, dirItemFullPath :: String}

instance Show DirItem where
  show item = (if dirItemIsDir item then "d " else "- ") <> dirItemName item

data AppState = AppState
  { _currentList :: L.List WidgetId DirItem,
    _parentDirList :: L.List WidgetId DirItem,
    _childDirList :: L.List WidgetId DirItem
  }

makeLenses ''AppState

rootDirFromArgs :: [String] -> String
rootDirFromArgs [] = "/home/kodus/testi/bulma"
rootDirFromArgs (arg : _) = arg

makeList :: WidgetId -> VE.Vector a -> L.List WidgetId a
makeList kind xs = L.list kind xs 3

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

-- TODO: rootDir must be full path!
createAppState :: FilePath -> IO AppState
createAppState rootDir = do
  currentDirItems <- getItems rootDir
  parentDirItems <- getItems (takeDirectory rootDir)
  let currentList' = makeList CurrentDirListKind currentDirItems
  return
    AppState
      { _currentList = currentList',
        _parentDirList = makeList ParentDirListKind parentDirItems,
        _childDirList = makeList ChildDirListId VE.empty
      }

renderDirItem :: Bool -> DirItem -> Widget a
renderDirItem _ item = str $ show item

renderApp :: AppState -> [Widget WidgetId]
renderApp appState =
  [ L.renderList renderDirItem False (appState ^. parentDirList)
      <+> L.renderList renderDirItem True (appState ^. currentList)
      <+> L.renderList renderDirItem False (appState ^. childDirList)
  ]

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.white `on` V.blue)
    ]

updateChildrenOnSelectedItem :: DirItem -> IO (L.List WidgetId DirItem)
updateChildrenOnSelectedItem dirItem =
  if dirItemIsDir dirItem
    then do
      items <- getItems (dirItemFullPath dirItem)
      return (makeList ChildDirListId items)
    else return (makeList ChildDirListId VE.empty)

updateListsAfterSelectedCHanged :: AppState -> IO AppState
updateListsAfterSelectedCHanged s = do
  let currList = s ^. currentList
  let selected = fmap snd (L.listSelectedElement currList)
  case selected of
    Just item -> do
      nextList <- updateChildrenOnSelectedItem item
      return (s {_childDirList = nextList})
    _ -> return s

appEvent :: T.BrickEvent WidgetId e -> T.EventM WidgetId AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey exitKey [] | exitKey `elem` [V.KEsc, V.KChar 'q'] -> M.halt
  ev -> do
    _ <- T.zoom currentList (L.handleListEventVi L.handleListEvent ev)
    s <- get
    liftIO (updateListsAfterSelectedCHanged s) >>= put
appEvent _ = return ()

theApp :: M.App AppState e WidgetId
theApp =
  M.App
    { M.appDraw = renderApp,
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
