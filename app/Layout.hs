module Layout (Layout (..), getListHeight, createLayout) where

import Graphics.Vty (Output (displayBounds), Vty (outputIface))

data Layout = Layout {getTerminalWidth :: Int, getTerminalHeight :: Int, topStatusBarHeight :: Int}

getListHeight :: Layout -> Int
getListHeight layout = getTerminalHeight layout - topStatusBarHeight layout

createLayout :: Vty -> IO Layout
createLayout vty = do
  (w, h) <- displayBounds $ outputIface vty
  return $
    Layout
      { getTerminalWidth = w,
        getTerminalHeight = h,
        topStatusBarHeight = 1
      }
