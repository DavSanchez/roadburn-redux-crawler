module Main where

import System.Directory
import VideoScraper (getRoadburnRedux)

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell Roadburn Redux scraper!"
  createDirectoryIfMissing False "roadburn-redux-21"
  getRoadburnRedux
