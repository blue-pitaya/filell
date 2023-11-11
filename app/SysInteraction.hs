module SysInteraction where

import Models.AppState (AbsolutePath, AppState (..), InteracviteList (InteracviteList, focusedIdx, getList), ListItem (ListItem, getName, getType), ListItemType (Dir, File), emptyInteractiveList, getCurrentListItem)
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

-- takeDirectory
createInteractiveList :: AbsolutePath -> IO InteracviteList
createInteractiveList path = getListOfPath path >>= (\l -> pure InteracviteList {getList = l, focusedIdx = 0})

updateLists :: AppState -> AbsolutePath -> IO AppState
updateLists state path = do
  currentList <- createInteractiveList path
  parentList <- createInteractiveList (takeDirectory path)
  childList <- getListForMaybe (getCurrentListItem (getCurrentList state))
  return state {getParentList = parentList, getCurrentList = currentList, getChildList = childList}
  where
    getListForMaybe (Just listItem) = createInteractiveList (path </> getName listItem)
    getListForMaybe Nothing = pure emptyInteractiveList
