module InteractiveList (InteractiveList (..), ListItem (..), ListItemType (..), getFocusedListItem, emptyInteractiveList, moveBy, getVisibleItems) where

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

data InteractiveList = InteractiveList
  { getList :: [ListItem],
    focusedIdx :: Int,
    -- for scrolling
    viewOffset :: Int
  }

getFocusedListItem :: InteractiveList -> Maybe ListItem
getFocusedListItem list = getList list `atMay` focusedIdx list

getVisibleItems :: Int -> InteractiveList -> [ListItem]
getVisibleItems viewHeight list = take viewHeight (drop (viewOffset list) (getList list))

emptyInteractiveList :: InteractiveList
emptyInteractiveList = InteractiveList {getList = [], focusedIdx = 0, viewOffset = 0}

moveBy :: Int -> Int -> InteractiveList -> InteractiveList
moveBy n listHeight list = list {focusedIdx = focusedIdx', viewOffset = viewOffset'}
  where
    focusedIdx' = min (max (focusedIdx list + n) 0) (length (getList list) - 1)
    viewOffsetChange
      | (focusedIdx' - viewOffset list) >= listHeight = 1
      | focusedIdx' < viewOffset list = -1
      | otherwise = 0
    viewOffset' = viewOffset list + viewOffsetChange
