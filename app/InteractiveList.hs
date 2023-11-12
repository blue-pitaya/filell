module InteractiveList (InteractiveList (..), ListItem (..), ListItemType (..), getCurrentListItem, emptyInteractiveList, moveBy) where

import Data.Ord (comparing)
import Safe (atMay)

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

data InteractiveList = InteractiveList
  {getList :: [ListItem], focusedIdx :: Int}

getCurrentListItem :: InteractiveList -> Maybe ListItem
getCurrentListItem list = getList list `atMay` focusedIdx list

emptyInteractiveList :: InteractiveList
emptyInteractiveList = InteractiveList {getList = [], focusedIdx = 0}

moveBy :: Int -> InteractiveList -> InteractiveList
moveBy n list = list {focusedIdx = focusedIdx'}
  where
    focusedIdx' = min (max (focusedIdx list + n) 0) (length (getList list) - 1)
