{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import Brick (Widget, simpleMain, str)

ui :: Widget ()
ui = str "Hello, Brick"

main :: IO ()
main = simpleMain ui
