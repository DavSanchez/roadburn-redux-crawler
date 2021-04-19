{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VideoScraper (getRoadburnRedux) where

import Data.Aeson (ToJSON (toJSON), encode, encodeFile, toEncoding)
import qualified Data.Text as T
import Network.HTTP.Simple
  ( getResponseBody,
    httpJSON,
    parseRequest,
  )
import Types
  ( DataItem (post_type, post_type_data),
    DataType,
    PostType,
    PostTypeDataItem (data_url),
    PostsItem (posts_data),
    URL,
  )

getRoadburnRedux :: IO ()
getRoadburnRedux = scrape >>= writeFileText "roadburn-redux-21/video-urls.txt" . unlines
-- OR getRoadburnRedux = scrape >>= traverse_ (appendFileText "data/roadburn-redux-urls.txt" . (<> "\n"))

req :: Text
req = "GET https://roadburn-api.lwprod.nl/api/posts"

queryString :: Int -> Text
queryString 0 = mempty
queryString n = "?limit=5&page=" <> show n

scrape :: IO [URL]
scrape = goScrape 0
  where
    goScrape n = do
      request <- parseRequest $ toString (req <> queryString n)
      response <- httpJSON request
      let scrapeData = getResponseBody response :: PostsItem
          videoLinks = (getVideoLinks . getVideoItems) scrapeData
      encodeFile ("roadburn-redux-21/contents-" ++ show n ++ ".json") scrapeData
      if (null . getPostData) scrapeData
        then pure []
        else pure videoLinks <> goScrape (n + 1) -- The IO monad is instance of Semigroup (?)

-- -----------------------------------
-- Data accessors and transformations
-- -----------------------------------

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