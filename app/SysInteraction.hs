module SysInteraction where

import Models.AppState (AbsolutePath, AppState (..), InteracviteList (InteracviteList, focusedIdx, getList), ListItem (ListItem, getName, getType), ListItemType (Dir, File), emptyInteractiveList, getCurrentListItem)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import Data.List (sort)

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

createInteractiveList :: AbsolutePath -> IO InteracviteList
createInteractiveList path = getListOfPath path >>= (\l -> pure InteracviteList {getList = sort l, focusedIdx = 0})

updateLists :: AppState -> IO AppState
updateLists state = updateCurrentList state >>= updateParentList >>= updateChildList

updateCurrentList :: AppState -> IO AppState
updateCurrentList state = do
  list <- createInteractiveList (currentAbsolutePath state)
  return state {getCurrentList = list}

updateParentList :: AppState -> IO AppState
updateParentList state = do
  list <- createInteractiveList (takeDirectory (currentAbsolutePath state))
  return state {getParentList = list}

updateChildList :: AppState -> IO AppState
updateChildList state = do
  list' <- getListForMaybe dirItem
  return (state {getChildList = list'})
  where
    dirItem = case getCurrentListItem (getCurrentList state) of
      Just x | getType x == Dir -> Just x
      _ -> Nothing
    getListForMaybe (Just listItem) = createInteractiveList (currentAbsolutePath state </> getName listItem)
    getListForMaybe Nothing = pure emptyInteractiveList
