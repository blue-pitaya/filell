module Main (main) where

import AppState (emptyAppState)
import EventHandler (eventLoop)
import Graphics.Vty
import SysInteraction (updateLists)

main :: IO ()
main = do
  let currentDirectory = "/home/kodus/testi/old"
  state <- updateLists (emptyAppState currentDirectory)
  vty <- mkVty defaultConfig
  _ <- eventLoop vty state
  shutdown vty

-- rootDirFromArgs :: [String] -> String
-- rootDirFromArgs [] = "/home/kodus/testi"
-- rootDirFromArgs (arg : _) = arg
--
-- getMimeType :: FullPath -> IO String
-- getMimeType fullPath = do
--  (exitCode, out, _) <- readProcessWithExitCode "file" ["-b", "--mime-type", fullPath] ""
--  case exitCode of
--    ExitSuccess -> return $ unpack $ strip $ pack out
--    _ -> return "?"
--
---- TODO: add default programs to open
-- openFileUsingMimeFormat :: FullPath -> IO ()
-- openFileUsingMimeFormat fullPath = do
--  mimeType <- getMimeType fullPath
--  mimeTypeFirstPart <- case splitOn "/" (pack mimeType) of
--    (x : _) -> return x
--    _ -> return "unknown"
--  openCommand <- case unpack mimeTypeFirstPart of
--    "audio" -> return "mpv"
--    "video" -> return "mpv"
--    "text" -> return "nvim"
--    _ -> return "xdg-open"
--  catch (callProcess openCommand [fullPath]) (\(_ :: SomeException) -> return ())
--
-- tryOpenFile :: AppState -> IO ()
-- tryOpenFile s = case L.listSelectedElement (currentList $ viewState s) of
--  Just (_, e) | not (dirItemIsDir e) -> do
--    openFileUsingMimeFormat (dirItemFullPath e)
--  _ -> return ()
--
