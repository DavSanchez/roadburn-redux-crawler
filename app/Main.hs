module Main where

import VideoScraper (getVideoUrls)

main :: IO ()
main = do
  putStrLn "Hello, Haskell Roadburn Redux scraper!"
  getVideoUrls
