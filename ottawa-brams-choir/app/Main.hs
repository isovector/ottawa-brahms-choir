{-# language OverloadedStrings #-}

module Main where

import Data.Foldable (for_)
import SitePipe
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Lens (_Text)

main :: IO ()
main = site $ do
  pages <- resourceLoader markdownReader ["pages/*.md"]

  for_ pages $ \page -> do
    let layout = page ^?! prop "layout" . jsonString
    writeTemplate (getTemplate layout) $ pure $ page
      & prop "url" . jsonString %~ drop 6

  copyFilesWith (drop 7) [ "static/*" ]

  pure ()

getTemplate :: String -> FilePath
getTemplate = ("templates/" ++) . (++ ".html")

------------------------------------------------------------------------------
-- | This is a prism but I couldn't figure out how to make it typecheck as
-- such.
prop :: Applicative f => Text -> (Value -> f Value) -> Value -> f Value
prop p = _Object . at p . _Just

jsonString :: Prism' Value String
jsonString = _String . _Text

