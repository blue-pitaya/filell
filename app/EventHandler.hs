module EventHandler (eventLoop) where

import AppState (AppState (..), getChildPath)
import Graphics.Vty (DisplayRegion, Event (EvKey), Key (KChar), Output (displayBounds), Vty (nextEvent, outputIface), picForImage, update)
import InteractiveList (moveBy)
import ListDrawing (getListHeight, imageForApp)
import SysInteraction (updateChildList, updateParentList)
import System.FilePath (takeDirectory)

handleEvent :: DisplayRegion -> AppState -> Event -> IO (Maybe AppState)
handleEvent _ _ (EvKey (KChar 'q') []) = return Nothing
handleEvent (_, terminalHeight) state (EvKey (KChar 'j') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy 1 (getListHeight terminalHeight) (getCurrentList state)
handleEvent (_, terminalHeight) state (EvKey (KChar 'k') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy (-1) (getListHeight terminalHeight) (getCurrentList state)
handleEvent _ state (EvKey (KChar 'h') []) = fmap Just nextState
  where
    parentPath = takeDirectory (currentAbsolutePath state)
    nextState =
      if parentPath /= currentAbsolutePath state
        then
          updateParentList
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

render :: Vty -> DisplayRegion -> AppState -> IO ()
render vty terminalSize state = do
  let mainImage = imageForApp state terminalSize
  update vty (picForImage mainImage)

eventLoop :: Vty -> AppState -> IO ()
eventLoop vty state = do
  terminalSize <- displayBounds $ outputIface vty
  _ <- render vty terminalSize state
  event <- nextEvent vty
  nextState <- handleEvent terminalSize state event
  resume nextState
  where
    resume (Just state') = eventLoop vty state'
    resume Nothing = return ()
