module EventHandler (eventLoop) where

import AppState (AppState (..), getChildPath)
import Graphics.Vty (Event (EvKey), Key (KChar), Vty (nextEvent), picForImage, update)
import InteractiveList (moveBy)
import Layout (Layout, createLayout, getListHeight)
import ListDrawing (imageForApp)
import SysInteraction (updateChildList, updateParentList)
import System.FilePath (takeDirectory)

handleEvent :: Layout -> AppState -> Event -> IO (Maybe AppState)
handleEvent _ _ (EvKey (KChar 'q') []) = return Nothing
handleEvent layout state (EvKey (KChar 'j') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy 1 (getListHeight layout) (getCurrentList state)
handleEvent layout state (EvKey (KChar 'k') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy (-1) (getListHeight layout) (getCurrentList state)
handleEvent layout state (EvKey (KChar 'h') []) = fmap Just nextState
  where
    parentPath = takeDirectory (currentAbsolutePath state)
    nextState =
      if parentPath /= currentAbsolutePath state
        then
          updateParentList
            layout
            state
              { currentAbsolutePath = parentPath,
                getCurrentList = getParentList state
              }
            >>= updateChildList -- TODO: maybe updating child list wont be necesary if root highlight is ok
        else pure state
handleEvent _ state (EvKey (KChar 'l') []) = fmap Just nextState
  where
    childPath = getChildPath state
    nextState =
      case childPath of
        Just path ->
          updateChildList
            state
              { currentAbsolutePath = path,
                getParentList = getCurrentList state,
                getCurrentList = getChildList state
              }
        Nothing -> pure state
handleEvent _ state _ = return (Just state)

render :: Vty -> Layout -> AppState -> IO ()
render vty layout state = do
  let mainImage = imageForApp layout state
  update vty (picForImage mainImage)

eventLoop :: Vty -> AppState -> IO ()
eventLoop vty state = do
  layout <- createLayout vty
  _ <- render vty layout state
  event <- nextEvent vty
  nextState <- handleEvent layout state event
  resume nextState
  where
    resume (Just state') = eventLoop vty state'
    resume Nothing = return ()
