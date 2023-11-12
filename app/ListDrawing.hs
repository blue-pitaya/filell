module ListDrawing (imageForApp) where

import Graphics.Vty (Attr, Image, defAttr, horizCat, resize, string, vertCat, vertJoin, withBackColor, withForeColor, withStyle)
-- explicit import for attributes
import qualified Graphics.Vty as Col
import Graphics.Vty.Image (char)
import Models.AppState (AppState (currentAbsolutePath, getChildList, getCurrentList, getParentList), InteracviteList (..), ListItem (ListItem, getName, getType), ListItemType (..))

type Size = (Int, Int)

type IsFocused = Bool

attrForListItem :: IsFocused -> ListItem -> Attr
attrForListItem False (ListItem {getType = File}) = defAttr `withForeColor` Col.white
attrForListItem True (ListItem {getType = File}) =
  defAttr `withForeColor` Col.black `withBackColor` Col.white
attrForListItem False (ListItem {getType = Dir}) =
  defAttr `withForeColor` Col.brightBlue `withStyle` Col.bold
attrForListItem True (ListItem {getType = Dir}) =
  defAttr `withForeColor` Col.black `withBackColor` Col.brightBlue `withStyle` Col.bold

imageForListItem :: IsFocused -> ListItem -> Image
imageForListItem isFocused item = string (attrForListItem isFocused item) (getName item)

imageForInteractiveList :: Size -> InteracviteList -> Image
imageForInteractiveList (width, height) interactiveList =
  resize width height listImage
  where
    _focusedIdx = focusedIdx interactiveList
    items = zipWith (\idx item -> (idx == _focusedIdx, item)) [0 ..] (getList interactiveList)
    listImage = vertCat $ map (uncurry imageForListItem) items

imageForCurrentPath :: FilePath -> Image
imageForCurrentPath = string (defAttr `withStyle` Col.bold `withForeColor` Col.brightMagenta)

-- '│' maybe column sign?
columnSeparator :: Int -> Image
columnSeparator height = vertCat (replicate height (char (defAttr `withForeColor` Col.brightBlack) ' '))

imageForApp :: AppState -> Size -> Image
imageForApp state (width, height) =
  pathImage `vertJoin` listsImage
  where
    -- TODO: when width is 92, then listWidth is 30, and there are 2 unused columsn
    -- write test that proves that all area is covered by lists
    -- min Width should be 30
    listWidth = width `div` 3
    pathImage = imageForCurrentPath (currentAbsolutePath state)
    toInteractiveList = imageForInteractiveList (listWidth, height)
    listsImage =
      horizCat
        [ toInteractiveList (getParentList state),
          columnSeparator height,
          toInteractiveList (getCurrentList state),
          columnSeparator height,
          toInteractiveList (getChildList state)
        ]
