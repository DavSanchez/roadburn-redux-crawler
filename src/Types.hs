{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))

type PostType = Text
type URL = Text
type DataType = Text

data PostsItem = PostsItem
  { fresh :: Bool,
    fresh_highlights :: Bool,
    fresh_dynamic_data :: Bool,
    posts_data :: [DataItem],
    links :: Links,
    meta :: Meta,
    highlights :: [Highlight]
  }
  deriving (Show, Generic, Eq)

instance FromJSON PostsItem where
  parseJSON = withObject "PostsItem" $ \obj ->
    PostsItem
      <$> obj .: "fresh"
      <*> obj .: "fresh_highlights"
      <*> obj .: "fresh_dynamic_data"
      <*> obj .: "data"
      <*> obj .: "links"
      <*> obj .: "meta"
      <*> obj .: "highlights"

data DataItem = DataItem
  { data_id :: Int,
    title :: Text,
    slug :: Text,
    content :: Text,
    post_type_status :: Text,
    post_type :: PostType,
    post_type_data :: PostTypeDataItem,
    category :: Maybe CategoryItem,
    likes_count :: Int,
    comments_count :: Int,
    highlight :: Int,
    active :: Int,
    published_at :: Text,
    time_start :: Maybe Text,
    time_end :: Maybe Text,
    created_at :: Text,
    updated_at :: Text,
    liked :: Bool
  }
  deriving (Show, Generic, Eq)

instance FromJSON DataItem where
  parseJSON = withObject "DataItem" $ \obj ->
    DataItem
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "slug"
      <*> obj .: "content"
      <*> obj .: "post_type_status"
      <*> obj .: "post_type"
      <*> obj .: "post_type_data"
      <*> obj .:? "category"
      <*> obj .: "likes_count"
      <*> obj .: "comments_count"
      <*> obj .: "highlight"
      <*> obj .: "active"
      <*> obj .: "published_at"
      <*> obj .:? "time_start"
      <*> obj .:? "time_end"
      <*> obj .: "created_at"
      <*> obj .: "updated_at"
      <*> obj .: "liked"

data PostTypeDataItem = PostTypeDataItem
  { data_type :: Maybe DataType,
    data_url :: Maybe Text,
    thumb_url :: Maybe Text,
    channel_id :: Maybe Text,
    meta_title :: Maybe Text,
    meta_image :: Maybe Text,
    images :: [ImageData]
  }
  deriving (Show, Generic, Eq)

instance FromJSON PostTypeDataItem where
  parseJSON = withObject "PostTypeDataItem" $ \obj ->
    PostTypeDataItem
      <$> obj .:? "type"
      <*> obj .:? "url"
      <*> obj .:? "thumb_url"
      <*> obj .:? "channel_id"
      <*> obj .:? "meta_title"
      <*> obj .:? "meta_image"
      <*> obj .: "images"

data ImageData = ImageData
  { image_url :: Text,
    name :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON ImageData where
  parseJSON = withObject "ImageData" $ \obj ->
    ImageData
      <$> obj .: "url"
      <*> obj .: "name"

data CategoryItem = CategoryItem
  { category_id :: Int,
    category_name :: Text,
    code :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON CategoryItem where
  parseJSON = withObject "CategoryItem" $ \obj ->
    CategoryItem
      <$> obj .: "id"
      <*> obj .: "name"
      <*> obj .: "code"

data Links = Links
  { first :: Text,
    last :: Text,
    prev :: Maybe Text,
    next :: Maybe Text
  }
  deriving (Show, Generic, Eq, FromJSON)

data Meta = Meta
  { current_page :: Int,
    from :: Int,
    last_page :: Int,
    path :: Text,
    per_page :: Int,
    to :: Int,
    total :: Int
  }
  deriving (Show, Generic, Eq, FromJSON)

type Highlight = DataItem