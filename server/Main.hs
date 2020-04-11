{-# OPTIONS_GHC -Wall #-}

module Main
  ( main,
  )
where

import qualified Whenever (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Whenever.someFunc
