module ListDrawing (imageForApp) where

import Graphics.Vty (Attr, Image, defAttr, horizCat, resize, string, vertCat, withBackColor, withForeColor)
-- explicit import for colors
import qualified Graphics.Vty as Col
import Models.AppState (AppState (getChildList, getCurrentList, getParentList), InteracviteList (..), ListItem (ListItem, getName, getType), ListItemType (..))

type Width = Int

type IsFocused = Bool

attrForListItem :: IsFocused -> ListItem -> Attr
attrForListItem False (ListItem {getType = File}) = defAttr `withForeColor` Col.white
attrForListItem True (ListItem {getType = File}) =
  defAttr `withForeColor` Col.black `withBackColor` Col.white
attrForListItem False (ListItem {getType = Dir}) = defAttr `withForeColor` Col.blue
attrForListItem True (ListItem {getType = Dir}) =
  defAttr `withForeColor` Col.black `withBackColor` Col.blue

imageForListItem :: IsFocused -> ListItem -> Image
imageForListItem isFocused item = string (attrForListItem isFocused item) (getName item)

imageForInteractiveList :: InteracviteList -> Width -> Image
imageForInteractiveList interactiveList width =
  resize width 30 listImage
  where
    _focusedIdx = focusedIdx interactiveList
    items = zipWith (\idx item -> (idx == _focusedIdx, item)) [0 ..] (getList interactiveList)
    listImage = vertCat $ map (uncurry imageForListItem) items

imageForApp :: AppState -> Width -> Image
imageForApp state width =
  horizCat $ map toInteractiveList lists
  where
    -- TODO: when width is 92, then listWidth is 30, and there are 2 unused columsn
    -- write test that proves that all area is covered by lists
    listWidth = width `div` 3
    lists = [getParentList state, getCurrentList state, getChildList state]
    toInteractiveList list = imageForInteractiveList list listWidth
