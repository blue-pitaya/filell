module ListDrawing where

import Graphics.Vty (Image, defAttr, green, horizCat, resize, string, vertCat, withForeColor)
import Models.AppState (AppState (getChildList, getCurrentList, getParentList), InteracviteList (getList), ListItem (getName))

type Width = Int

type LeftMargin = Int

widgetForListItem :: ListItem -> Image
widgetForListItem item = string (defAttr `withForeColor` green) (getName item)

imageForInteractiveList :: InteracviteList -> Width -> Image
imageForInteractiveList interactiveList width =
  resize width 30 listImage
  where
    items = getList interactiveList
    listImage = vertCat $ map widgetForListItem items

imageForApp :: AppState -> Width -> Image
imageForApp state width =
  horizCat $ map toInteractiveList lists
  where
    -- TODO: when width is 92, then listWidth is 30, and there are 2 unused columsn
    -- write test that proves that all area is covered by lists
    listWidth = width `div` 3
    lists = [getParentList state, getCurrentList state, getChildList state]
    toInteractiveList list = imageForInteractiveList list listWidth
