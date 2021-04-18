{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VideoScraper (getVideoUrls) where

import Data.Aeson ()
import Network.HTTP.Simple
  ( getResponseBody,
    httpJSON,
    parseRequest,
  )
import Types (DataItem (post_type, post_type_data), DataType, PostType, PostTypeDataItem (data_url), PostsItem (posts_data), URL)

req :: String
req = "GET https://roadburn-api.lwprod.nl/api/posts"

queryString :: String -> String
queryString = (++) "?limit=5&page="

getVideoUrls :: IO ()
getVideoUrls = scrape >>= (print . getVideoLinks . getVideoItems)

scrape :: IO PostsItem
scrape = do
  request <- parseRequest req
  response <- httpJSON request
  let scrapeData = getResponseBody response :: PostsItem
  pure scrapeData

getVideoLinks :: [PostTypeDataItem] -> [URL]
getVideoLinks = mapMaybe getVideoUrl

getVideoItems :: PostsItem -> [PostTypeDataItem]
getVideoItems = getDataItems . filterByVideo . getPostData

getPostData :: PostsItem -> [DataItem]
getPostData = posts_data

getPostTypeFromData :: DataItem -> PostType
getPostTypeFromData = post_type

isVideo :: PostType -> Bool
isVideo = (==) "video"

filterByVideo :: [DataItem] -> [DataItem]
filterByVideo = filter (isVideo . getPostTypeFromData)

isFromVimeo :: DataType -> Bool
isFromVimeo = (==) "VIMEO"

getVideoUrl :: PostTypeDataItem -> Maybe URL
getVideoUrl = data_url

getPostTypeDataItem :: DataItem -> PostTypeDataItem
getPostTypeDataItem = post_type_data

getDataItems :: [DataItem] -> [PostTypeDataItem]
getDataItems = map getPostTypeDataItem