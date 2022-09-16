{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( -- * Synopsis
    -- $synopsis
    MappedText(..)
  , demo
  ) where

import Data.String (IsString(..))
import Data.Text (Text, pack)
import qualified Data.Text as Text

newtype MappedText = MappedText
  { getMappedText :: (Char -> Char) -> Text
  }

instance Semigroup MappedText where
  mt1 <> mt2 = MappedText $ \f -> getMappedText mt1 f <> getMappedText mt2 f

instance IsString MappedText where
  fromString s = MappedText $ \f -> Text.map f $ pack s

-- | Demo that can be run in @ghci@:
--
-- > λ> demo id
-- > "functions always prevail!"
-- > λ> demo Data.Char.toUpper
-- > "FUNCTIONS ALWAYS PREVAIL!"
demo :: (Char -> Char) -> IO ()
demo = print . getMappedText mappedText
  where
  mappedText :: MappedText
  mappedText = "functions " <> "always " <> "prevail!"

-- $synopsis
--
-- Often times, we need some data determined at runtime to drive our program's
-- behavior. When we need to thread configuration through our programs,
-- @ReaderT@ may come to mind. We can use the same idea behind @ReaderT@ in all
-- sorts of ways.
--
-- We demonstrate one example of this pattern by introducing a 'MappedText' data
-- type. This type wraps a 'Text', but has a config parameter capturing a
-- character-level transformation we'd like to eventually apply to the wrapped
-- 'Text'.
--
-- This approach is particularly powerful considering that we are able to
-- leverage the transformation function in instance definitions (see the
-- 'Semigroup' and 'IsString' examples), even though the transformation function
-- isn't concretely specified until runtime.
