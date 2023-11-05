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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor
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

data AppViewState = DefaultViewState
  { currentList :: L.List WidgetId DirItem,
    parentDirList :: L.List WidgetId DirItem,
    childDirList :: L.List WidgetId DirItem
  }

data AppState = AppState
  { viewState :: AppViewState,
    stateCurrentDir :: FullPath
  }

rootDirFromArgs :: [String] -> String
rootDirFromArgs [] = "/home/kodus/testi/bulma"
rootDirFromArgs (arg : _) = arg

makeList :: WidgetId -> VE.Vector a -> L.List WidgetId a
makeList kind xs = L.list kind xs 3

fromPath :: FilePath -> FilePath -> IO DirItem
fromPath dirRoot filePath = do
  let fullPath' = dirRoot </> filePath
  isDir' <- doesDirectoryExist fullPath'
  return DirItem {dirItemIsDir = isDir', dirItemName = filePath, dirItemFullPath = fullPath'}

getItems :: FilePath -> IO (VE.Vector DirItem)
getItems dirPath = fmap VE.fromList (listDirectory dirPath >>= mapM (fromPath dirPath))

-- TODO: rootDir must be full path!
createAppState :: FilePath -> IO AppState
createAppState rootDir = do
  lists <- renderLists rootDir
  return AppState {stateCurrentDir = rootDir, viewState = lists}

renderLists :: FullPath -> IO AppViewState
renderLists d = do
  currentDirItems <- getItems d
  parentDirItems <- getItems (takeDirectory d)
  let currentList' = makeList CurrentDirListKind currentDirItems
  let viewState' =
        DefaultViewState
          { currentList = currentList',
            parentDirList = makeList ParentDirListKind parentDirItems,
            childDirList = makeList ChildDirListId VE.empty
          }
  return viewState'

renderDirItem :: Bool -> DirItem -> Widget a
renderDirItem _ item = str $ show item

renderApp :: AppState -> [Widget WidgetId]
renderApp appState =
  [ L.renderList renderDirItem False ((parentDirList . viewState) appState)
      <+> L.renderList renderDirItem True ((currentList . viewState) appState)
      <+> L.renderList renderDirItem False ((childDirList . viewState) appState)
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

updateListsAfterSelectedChanged :: AppViewState -> IO AppViewState
updateListsAfterSelectedChanged s = do
  let currList = currentList s
  let selected = fmap snd (L.listSelectedElement currList)
  case selected of
    Just item -> do
      nextList <- updateChildrenOnSelectedItem item
      return (s {childDirList = nextList})
    _ -> return s

-- TODO: I guess this should be defined as HorizonalDirSwitchAction :)
handleGoParent :: AppState -> IO AppState
handleGoParent s = do
  let parentDir = takeDirectory (stateCurrentDir s)
  nextLists <- renderLists parentDir
  return s {stateCurrentDir = parentDir, viewState = nextLists}

handleGoInsideDir :: AppState -> IO AppState
handleGoInsideDir s = case L.listSelectedElement (currentList $ viewState s) of
  Just (_, e) | dirItemIsDir e -> do
    let nextDir = dirItemFullPath e
    nextLists <- renderLists nextDir
    return s {stateCurrentDir = nextDir, viewState = nextLists}
  _ -> return s

-- TODO: can be single list
handleVerticalMovements :: AppViewState -> Kleisli Maybe V.Event AppViewState
handleVerticalMovements s =
  Kleisli
    ( \case
        V.EvKey (V.KChar 'j') [] -> Just $ s {currentList = L.listMoveDown (currentList s)}
        V.EvKey (V.KChar 'k') [] -> Just $ s {currentList = L.listMoveUp (currentList s)}
        -- TODO: Change to keybinding "gg"
        V.EvKey (V.KChar 'g') [] -> Just $ s {currentList = L.listMoveToBeginning (currentList s)}
        V.EvKey (V.KChar 'G') [] -> Just $ s {currentList = L.listMoveToEnd (currentList s)}
        _ -> Nothing
    )

-- TODO: dont update child list if selection didnt changed
updateAppState :: AppState -> V.Event -> IO AppState
updateAppState s ev = do
  let nextViewState = fromMaybe (viewState s) (runKleisli (handleVerticalMovements (viewState s)) ev)
  nextViewState' <- updateListsAfterSelectedChanged nextViewState
  return s {viewState = nextViewState'}

appEvent :: T.BrickEvent WidgetId e -> T.EventM WidgetId AppState ()
appEvent (T.VtyEvent e) = case e of
  V.EvKey exitKey [] | exitKey `elem` [V.KEsc, V.KChar 'q'] -> M.halt
  V.EvKey (V.KChar 'h') [] -> do
    s <- get
    liftIO (handleGoParent s) >>= put
  V.EvKey (V.KChar 'l') [] -> do
    s <- get
    liftIO (handleGoInsideDir s) >>= put
  ev -> do
    s <- get
    liftIO (updateAppState s ev) >>= put
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
