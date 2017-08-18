module Main where

import System.Directory (doesFileExist, getHomeDirectory)

import UI (run)

main :: IO ()
main = do
  home <- getHomeDirectory
  let saveFilePath = home ++ "/.tofe"
  saveFileExists <- doesFileExist saveFilePath
  best <- if saveFileExists
    then do
      saveData <- readFile saveFilePath
      return $ read saveData
    else return 0
  score <- run best
  writeFile saveFilePath $ show $ max score best
  return ()
