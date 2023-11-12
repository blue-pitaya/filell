module EventHandler (eventLoop) where

import Graphics.Vty (Event (EvKey), Key (KChar), Output (displayBounds), Vty (nextEvent, outputIface), picForImage, update)
import ListDrawing (imageForApp)
import Models.AppState (AppState (..), moveBy)
import SysInteraction (updateChildList, updateParentList)
import System.FilePath (takeDirectory)

handleEvent :: AppState -> Event -> IO (Maybe AppState)
handleEvent _ (EvKey (KChar 'q') []) = return Nothing
handleEvent state (EvKey (KChar 'j') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy 1 (getCurrentList state)
handleEvent state (EvKey (KChar 'k') []) = do
  state' <- updateChildList state {getCurrentList = list'}
  return $ Just state'
  where
    list' = moveBy (-1) (getCurrentList state)
handleEvent state (EvKey (KChar 'h') []) = fmap Just nextState
  where
    parentPath = takeDirectory (currentAbsolutePath state)
    nextState =
      if parentPath /= currentAbsolutePath state
        then
          updateParentList
            state
              { currentAbsolutePath = takeDirectory (currentAbsolutePath state),
                getCurrentList = getParentList state,
                getChildList = getCurrentList state
              }
        else pure state
handleEvent state _ = return (Just state)

render :: Vty -> AppState -> IO ()
render vty state = do
  (width, _) <- displayBounds $ outputIface vty
  let mainImage = imageForApp state width
  update vty (picForImage mainImage)

eventLoop :: Vty -> AppState -> IO ()
eventLoop vty state = do
  _ <- render vty state
  event <- nextEvent vty
  nextState <- handleEvent state event
  resume nextState
  where
    resume (Just state') = eventLoop vty state'
    resume Nothing = return ()
