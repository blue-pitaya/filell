module Models.AppState where

import Data.Ord (comparing)
import Safe (atMay)

type AbsolutePath = FilePath

data InteracviteList = InteracviteList
  {getList :: [ListItem], focusedIdx :: Int}

getCurrentListItem :: InteracviteList -> Maybe ListItem
getCurrentListItem list = getList list `atMay` focusedIdx list

emptyInteractiveList :: InteracviteList
emptyInteractiveList = InteracviteList {getList = [], focusedIdx = 0}

moveBy :: Int -> InteracviteList -> InteracviteList
moveBy n list = list {focusedIdx = focusedIdx'}
  where
    focusedIdx' = min (max (focusedIdx list + n) 0) (length (getList list) - 1)

data AppState = AppState
  { currentAbsolutePath :: FilePath,
    getParentList :: InteracviteList,
    getCurrentList :: InteracviteList,
    getChildList :: InteracviteList
  }

emptyAppState :: AbsolutePath -> AppState
emptyAppState path =
  AppState
    { currentAbsolutePath = path,
      getParentList = emptyInteractiveList,
      getCurrentList = emptyInteractiveList,
      getChildList = emptyInteractiveList
    }

data ListItemType = File | Dir deriving (Eq)

instance Ord ListItemType where
  compare Dir File = LT
  compare File Dir = GT
  compare _ _ = EQ

data ListItem = ListItem
  { getName :: String,
    getType :: ListItemType
  }
  deriving (Eq)

instance Ord ListItem where
  compare = comparing (\x -> (getType x, getName x))

instance Show ListItem where
  show item = unwords [typePart, getName item]
    where
      isDir = getType item == Dir
      typePart = if isDir then "d" else "-"
