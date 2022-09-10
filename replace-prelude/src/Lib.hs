{-# LANGUAGE OverloadedStrings #-}

module Lib where

someFunc :: IO ()
someFunc = do
  -- We can now access types that are in RIO but not in Prelude
  -- like Text and ByteString
  let text = "asdf" :: Text
      bs = "asdf" :: ByteString

  pure ()
