module SysInteraction where

import AppState (AbsolutePath, AppState (..))
import Data.List (findIndex, sort)
import Data.Maybe (fromMaybe)
import InteractiveList (InteractiveList (..), ListItem (..), ListItemType (Dir, File), emptyInteractiveList, getFocusedListItem, setFocusedIdx)
import Layout (Layout, getListHeight)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, (</>))

itemFromPath :: AbsolutePath -> FilePath -> IO ListItem
itemFromPath rootPath path = do
  let fullPath = rootPath </> path
  isDir <- doesDirectoryExist fullPath
  return
    ListItem
      { getName = path,
        getType = if isDir then Dir else File
      }

getListOfPath :: AbsolutePath -> IO [ListItem]
getListOfPath path = do
  contents <- listDirectory path
  mapM (itemFromPath path) contents

startsWith :: Char -> String -> Bool
startsWith c (x : _) = c == x
startsWith _ _ = False

createInteractiveList :: AbsolutePath -> Bool -> IO InteractiveList
createInteractiveList path showHidden = do
  items <- getListOfPath path
  let filteredItems = if showHidden then items else filter (not . startsWith '.' . getName) items
  return (emptyInteractiveList {getList = sort filteredItems})

-- TODO: try perserve focused on same element
updateLists :: Layout -> AppState -> IO AppState
updateLists layout state = updateCurrentList state >>= updateParentList layout >>= updateChildList

updateCurrentList :: AppState -> IO AppState
updateCurrentList state = do
  list <- createInteractiveList (currentAbsolutePath state) (areHiddenFilesVisible state)
  return state {getCurrentList = list}

updateParentList :: Layout -> AppState -> IO AppState
updateParentList layout state = do
  let parentDirPath = takeDirectory (currentAbsolutePath state)
  list <- createInteractiveList parentDirPath (areHiddenFilesVisible state)
  let items = getList list
  let maybeFocusedIdx = findIndex (\item -> parentDirPath </> getName item == currentAbsolutePath state) items
  let focusedIdx' = fromMaybe 0 maybeFocusedIdx
  return state {getParentList = setFocusedIdx focusedIdx' (getListHeight layout) list}

updateChildList :: AppState -> IO AppState
updateChildList state = do
  list' <- getListForMaybe dirItem (areHiddenFilesVisible state)
  return (state {getChildList = list'})
  where
    dirItem = case getFocusedListItem (getCurrentList state) of
      Just x | getType x == Dir -> Just x
      _ -> Nothing
    getListForMaybe (Just listItem) x = createInteractiveList (currentAbsolutePath state </> getName listItem) x
    getListForMaybe Nothing _ = pure emptyInteractiveList
