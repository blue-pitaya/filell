{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Brick
import qualified Brick.AttrMap as A
import Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Arrow (Kleisli (Kleisli, runKleisli))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as VE
import qualified Graphics.Vty as V
import System.Directory
import System.Environment
import System.FilePath

data WidgetId = ParentDirListKind | CurrentDirListKind | ChildDirListId deriving (Eq, Ord, Show)

data DirItem = DirItem {dirItemIsDir :: Bool, dirItemName :: String, dirItemFullPath :: String}

instance Show DirItem where
  show item = (if dirItemIsDir item then "d " else "- ") <> dirItemName item

type FullPath = FilePath

data AppState = AppState
  { currentList :: L.List WidgetId DirItem,
    parentDirList :: L.List WidgetId DirItem,
    childDirList :: L.List WidgetId DirItem,
    stateCurrentDir :: FullPath
  }

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
      { currentList = currentList',
        parentDirList = makeList ParentDirListKind parentDirItems,
        childDirList = makeList ChildDirListId VE.empty,
        stateCurrentDir = rootDir
      }

renderLists :: AppState -> IO AppState
renderLists s = do
  let d = stateCurrentDir s
  currentDirItems <- getItems d
  parentDirItems <- getItems (takeDirectory d)
  let currentList' = makeList CurrentDirListKind currentDirItems
  return
    s
      { currentList = currentList',
        parentDirList = makeList ParentDirListKind parentDirItems,
        childDirList = makeList ChildDirListId VE.empty
      }

renderDirItem :: Bool -> DirItem -> Widget a
renderDirItem _ item = str $ show item

renderApp :: AppState -> [Widget WidgetId]
renderApp appState =
  [ L.renderList renderDirItem False (parentDirList appState)
      <+> L.renderList renderDirItem True (currentList appState)
      <+> L.renderList renderDirItem False (childDirList appState)
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

updateListsAfterSelectedChanged :: AppState -> IO AppState
updateListsAfterSelectedChanged s = do
  let currList = currentList s
  let selected = fmap snd (L.listSelectedElement currList)
  case selected of
    Just item -> do
      nextList <- updateChildrenOnSelectedItem item
      return (s {childDirList = nextList})
    _ -> return s

handleGoParent :: AppState -> IO AppState
handleGoParent s = do
  let parentDir = takeDirectory (stateCurrentDir s)
  let nextState = s {stateCurrentDir = parentDir}
  renderLists nextState

handleVertChuj :: AppState -> Kleisli Maybe V.Event AppState
handleVertChuj s =
  Kleisli
    ( \case
        V.EvKey (V.KChar 'j') [] -> Just $ s {currentList = L.listMoveBy 1 (currentList s)}
        V.EvKey (V.KChar 'k') [] -> Just $ s {currentList = L.listMoveBy (-1) (currentList s)}
        -- TODO: "gg"
        V.EvKey (V.KChar 'G') [] -> Just s
        _ -> Nothing
    )

--    Up (k)
--    Down (j)
--    Page Up (Ctrl-b)
--    Page Down (Ctrl-f)
--    Half Page Up (Ctrl-u)
--    Half Page Down (Ctrl-d)
--    Go to first element (g)
--    Go to last element (G)

appEvent :: T.BrickEvent WidgetId e -> T.EventM WidgetId AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey exitKey [] | exitKey `elem` [V.KEsc, V.KChar 'q'] -> M.halt
  V.EvKey (V.KChar 'h') [] -> do
    s <- get
    liftIO (handleGoParent s) >>= put
  ev -> do
    s <- get
    let nextState = fromMaybe s (runKleisli (handleVertChuj s) ev)
    let nextState' = updateListsAfterSelectedChanged nextState
    liftIO nextState' >>= put
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
