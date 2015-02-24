module Main where

import Boggle

import Control.Monad

main :: IO ()
main = liftM (trayString '_') (shuffledTray (5,5)) >>= putStrLn
