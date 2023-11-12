module AppState where

import InteractiveList (InteractiveList, ListItem (..), ListItemType (..), emptyInteractiveList, getFocusedListItem)
import System.FilePath ((</>))

type AbsolutePath = FilePath

data AppState = AppState
  { currentAbsolutePath :: FilePath,
    getParentList :: InteractiveList,
    getCurrentList :: InteractiveList,
    getChildList :: InteractiveList,
    areHiddenFilesVisible :: Bool
  }

emptyAppState :: AbsolutePath -> AppState
emptyAppState path =
  AppState
    { currentAbsolutePath = path,
      getParentList = emptyInteractiveList,
      getCurrentList = emptyInteractiveList,
      getChildList = emptyInteractiveList,
      areHiddenFilesVisible = False
    }

getChildPath :: AppState -> Maybe AbsolutePath
getChildPath state = do
  item <- getFocusedListItem (getCurrentList state)
  dirItem <- if getType item == Dir then Just item else Nothing
  return (currentAbsolutePath state </> getName dirItem)
