module Main where

import Boggle

import Control.Monad
import Data.List

main :: IO ()
main = do
  (shuffledTray (4,4)) >>= \tray ->
    putStrLn $ intercalate "\n" $ [trayString '_' tray, ""] ++ wordsOnTray tray threeLetterWords
