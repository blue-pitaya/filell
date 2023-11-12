module InteractiveList (InteractiveList (..), ListItem (..), ListItemType (..), getFocusedListItem, emptyInteractiveList, moveBy, getVisibleItems, setFocusedIdx) where

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

setFocusedIdx :: Int -> Int -> InteractiveList -> InteractiveList
setFocusedIdx n listHeight list = moveBy delta listHeight list
  where
    delta = n - focusedIdx list

moveBy :: Int -> Int -> InteractiveList -> InteractiveList
moveBy n listHeight list = list {focusedIdx = focusedIdx', viewOffset = viewOffset'}
  where
    focusedIdx' = min (max (focusedIdx list + n) 0) (length (getList list) - 1)
    -- both bounds are inclusive
    lowerViewBound = viewOffset list
    upperViewBound = lowerViewBound + listHeight - 1
    viewOffsetChange
      | (focusedIdx' - upperViewBound) > 0 = focusedIdx' - upperViewBound
      | (focusedIdx' - lowerViewBound) < 0 = focusedIdx' - lowerViewBound
      | otherwise = 0
    viewOffset' = viewOffset list + viewOffsetChange
