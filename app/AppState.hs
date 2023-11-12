module AppState where

import InteractiveList (InteractiveList (..), ListItem (..), ListItemType (..), emptyInteractiveList, getCurrentListItem)
import System.FilePath ((</>))

type AbsolutePath = FilePath

data AppState = AppState
  { currentAbsolutePath :: FilePath,
    getParentList :: InteractiveList,
    getCurrentList :: InteractiveList,
    getChildList :: InteractiveList
  }

emptyAppState :: AbsolutePath -> AppState
emptyAppState path =
  AppState
    { currentAbsolutePath = path,
      getParentList = emptyInteractiveList,
      getCurrentList = emptyInteractiveList,
      getChildList = emptyInteractiveList
    }

getChildPath :: AppState -> Maybe AbsolutePath
getChildPath state = do
  item <- getCurrentListItem (getCurrentList state)
  dirItem <- if getType item == Dir then Just item else Nothing
  return (currentAbsolutePath state </> getName dirItem)
