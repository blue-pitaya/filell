{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: remove this later
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Graphics.Vty
import ListDrawing (imageForApp)
import Models.AppState (emptyAppState)
import SysInteraction (updateLists)
import System.Directory (getCurrentDirectory)

-- import Brick
-- import qualified Brick.AttrMap as A
-- import Brick.Main as M
-- import qualified Brick.Types as T
-- import qualified Brick.Widgets.List as L
-- import Control.Arrow (Kleisli (Kleisli, runKleisli))
-- import Control.Exception
-- import Control.Monad.IO.Class (MonadIO (liftIO))
-- import Data.Functor
-- import Data.Maybe (fromMaybe)
-- import Data.Text (pack, splitOn, strip, unpack)
-- import qualified Data.Vector as VE
-- import qualified Graphics.Vty as V
-- import System.Directory
-- import System.Environment
-- import System.Exit
-- import System.FilePath
-- import System.Process
--
-- type MimeType = String
--
-- data WidgetId = ParentDirListKind | CurrentDirListKind | ChildDirListId deriving (Eq, Ord, Show)
--
-- data DirItem = DirItem
--  { dirItemIsDir :: Bool,
--    dirItemName :: String,
--    dirItemFullPath :: String,
--    dirItemMimeType :: MimeType
--  }
--
-- instance Show DirItem where
--  show item = unwords (filter (not . null) [typePart, dirItemName item, mimeTypePart])
--    where
--      isDir = dirItemIsDir item
--      typePart = if isDir then "d" else "-"
--      mimeTypePart = if not isDir then "[" <> dirItemMimeType item <> "]" else ""
--
-- type FullPath = FilePath
--
-- data AppViewState = DefaultViewState
--  { currentList :: L.List WidgetId DirItem,
--    parentDirList :: L.List WidgetId DirItem,
--    childDirList :: L.List WidgetId DirItem
--  }
--
-- data AppState = AppState
--  { viewState :: AppViewState,
--    stateCurrentDir :: FullPath
--  }
--
-- rootDirFromArgs :: [String] -> String
-- rootDirFromArgs [] = "/home/kodus/testi"
-- rootDirFromArgs (arg : _) = arg
--
-- makeList :: WidgetId -> VE.Vector a -> L.List WidgetId a
-- makeList kind xs = L.list kind xs 1
--
-- getMimeType :: FullPath -> IO String
-- getMimeType fullPath = do
--  (exitCode, out, _) <- readProcessWithExitCode "file" ["-b", "--mime-type", fullPath] ""
--  case exitCode of
--    ExitSuccess -> return $ unpack $ strip $ pack out
--    _ -> return "?"
--
---- TODO: add default programs to open
-- openFileUsingMimeFormat :: FullPath -> IO ()
-- openFileUsingMimeFormat fullPath = do
--  mimeType <- getMimeType fullPath
--  mimeTypeFirstPart <- case splitOn "/" (pack mimeType) of
--    (x : _) -> return x
--    _ -> return "unknown"
--  openCommand <- case unpack mimeTypeFirstPart of
--    "audio" -> return "mpv"
--    "video" -> return "mpv"
--    "text" -> return "nvim"
--    _ -> return "xdg-open"
--  catch (callProcess openCommand [fullPath]) (\(_ :: SomeException) -> return ())
--
-- fromPath :: FilePath -> FilePath -> IO DirItem
-- fromPath dirRoot filePath = do
--  isDir <- doesDirectoryExist fullPath
--  -- mimeType <- getMimeType fullPath
--  return
--    DirItem
--      { dirItemIsDir = isDir,
--        dirItemName = filePath,
--        dirItemFullPath = fullPath,
--        dirItemMimeType = ""
--      }
--  where
--    fullPath = dirRoot </> filePath
--
-- getItems :: FilePath -> IO (VE.Vector DirItem)
-- getItems dirPath = fmap VE.fromList (listDirectory dirPath >>= mapM (fromPath dirPath))
--
---- TODO: rootDir must be full path!
-- createAppState :: FilePath -> IO AppState
-- createAppState rootDir = do
--  lists <- renderLists rootDir
--  return AppState {stateCurrentDir = rootDir, viewState = lists}
--
---- TODO: this is not rendering lists, its just creating them
-- renderLists :: FullPath -> IO AppViewState
-- renderLists d = do
--  currentDirItems <- getItems d
--  parentDirItems <- getItems (takeDirectory d)
--  let currentList' = makeList CurrentDirListKind currentDirItems
--  let viewState' =
--        DefaultViewState
--          { currentList = currentList',
--            parentDirList = makeList ParentDirListKind parentDirItems,
--            childDirList = makeList ChildDirListId VE.empty
--          }
--  return viewState'
--
-- renderApp :: AppState -> [Widget WidgetId]
-- renderApp appState =
--  [ L.renderList renderItem False ((parentDirList . viewState) appState)
--      <+> L.renderList renderItem True ((currentList . viewState) appState)
--      <+> L.renderList renderItem False ((childDirList . viewState) appState)
--  ]
--  where
--    renderItem _ item = str (show item)
--
-- updateChildrenOnSelectedItem :: DirItem -> IO (L.List WidgetId DirItem)
-- updateChildrenOnSelectedItem dirItem =
--  if dirItemIsDir dirItem
--    then do
--      items <- getItems (dirItemFullPath dirItem)
--      return (makeList ChildDirListId items)
--    else return (makeList ChildDirListId VE.empty)
--
-- updateListsAfterSelectedChanged :: AppViewState -> IO AppViewState
-- updateListsAfterSelectedChanged s = do
--  let currList = currentList s
--  let selected = fmap snd (L.listSelectedElement currList)
--  case selected of
--    Just item -> do
--      nextList <- updateChildrenOnSelectedItem item
--      return (s {childDirList = nextList})
--    _ -> return s
--
---- TODO: I guess this should be defined as HorizonalDirSwitchAction :)
-- handleGoParent :: AppState -> IO AppState
-- handleGoParent s = do
--  let parentDir = takeDirectory (stateCurrentDir s)
--  nextLists <- renderLists parentDir
--  return s {stateCurrentDir = parentDir, viewState = nextLists}
--
-- handleGoInsideDir :: AppState -> IO AppState
-- handleGoInsideDir s = case L.listSelectedElement (currentList $ viewState s) of
--  Just (_, e) | dirItemIsDir e -> do
--    let nextDir = dirItemFullPath e
--    nextLists <- renderLists nextDir
--    return s {stateCurrentDir = nextDir, viewState = nextLists}
--  _ -> return s
--
-- tryOpenFile :: AppState -> IO ()
-- tryOpenFile s = case L.listSelectedElement (currentList $ viewState s) of
--  Just (_, e) | not (dirItemIsDir e) -> do
--    openFileUsingMimeFormat (dirItemFullPath e)
--  _ -> return ()
--
---- TODO: can be single list
-- handleVerticalMovements :: AppViewState -> Kleisli Maybe V.Event AppViewState
-- handleVerticalMovements s =
--  Kleisli
--    ( \case
--        V.EvKey (V.KChar 'j') [] -> Just $ s {currentList = L.listMoveDown (currentList s)}
--        V.EvKey (V.KChar 'k') [] -> Just $ s {currentList = L.listMoveUp (currentList s)}
--        -- TODO: Change to keybinding "gg"
--        V.EvKey (V.KChar 'g') [] -> Just $ s {currentList = L.listMoveToBeginning (currentList s)}
--        V.EvKey (V.KChar 'G') [] -> Just $ s {currentList = L.listMoveToEnd (currentList s)}
--        _ -> Nothing
--    )
--
---- TODO: dont update child list if selection didnt changed
-- updateAppState :: AppState -> V.Event -> IO AppState
-- updateAppState s ev = do
--  let nextViewState = fromMaybe (viewState s) (runKleisli (handleVerticalMovements (viewState s)) ev)
--  nextViewState' <- updateListsAfterSelectedChanged nextViewState
--  return s {viewState = nextViewState'}
--
-- appEvent :: T.BrickEvent WidgetId e -> T.EventM WidgetId AppState ()
-- appEvent (T.VtyEvent event) = case event of
--  V.EvKey exitKey [] | exitKey `elem` [V.KEsc, V.KChar 'q'] -> M.halt
--  V.EvKey (V.KChar 'h') [] -> get >>= liftIO . handleGoParent >>= put
--  V.EvKey (V.KChar 'l') [] -> get >>= liftIO . handleGoInsideDir >>= put
--  V.EvKey V.KEnter [] -> get >>= suspendAndResume' . tryOpenFile
--  e -> get >>= \s -> liftIO (updateAppState s e) >>= put
-- appEvent _ = return ()
--
-- theApp :: M.App AppState () WidgetId
-- theApp =
--  M.App
--    { M.appDraw = renderApp,
--      M.appChooseCursor = M.showFirstCursor,
--      M.appHandleEvent = appEvent,
--      M.appStartEvent = return (),
--      M.appAttrMap =
--        const
--          ( A.attrMap
--              V.defAttr
--              [ (L.listAttr, V.white `on` V.black),
--                (L.listSelectedAttr, V.white `on` V.blue)
--              ]
--          )
--    }

-- main :: IO ()
-- main = do
--  args <- getArgs
--  let rootDir = rootDirFromArgs args
--  initialState <- createAppState rootDir
--  void $ M.defaultMain theApp initialState
main :: IO ()
main = do
  let currentDirectory = "/home/kodus/testi/old"
  state <- updateLists (emptyAppState currentDirectory) currentDirectory
  vty <- mkVty defaultConfig
  -- Get the size of the terminal
  (width, _) <- displayBounds $ outputIface vty
  let mainImage = imageForApp state width
  -- Display the centered image
  update vty (picForImage mainImage)
  -- Wait for the "q" key to exit
  event <- nextEvent vty
  case event of
    EvKey (KChar 'q') [] -> return ()
    _ -> main
  -- Shutdown the Vty interface
  shutdown vty
