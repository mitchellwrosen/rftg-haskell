module Main where

import System.IO
import System.Directory

copyFiles _ [] = return ()
copyFiles (from:froms) (to:tos) = do
   copyFile from (to ++ ".jpg")
   copyFiles froms tos

main = do
   fromStr <- readFile "rename.from"
   toStr <- readFile "rename.to"
   let fromNames = lines fromStr
       toNames = lines toStr
   copyFiles fromNames toNames
