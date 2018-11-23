{-# LANGUAGE LambdaCase        #-}
{-# language OverloadedStrings #-}

module Main where

import Data.Bool (bool)
import Debug.Trace
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable (for_)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Text.Lens (_Text)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Parse (strptime)
import SitePipe

main :: IO ()
main = site $ do
  pages <- resourceLoader markdownReader ["pages/*.md"]

  for_ pages $ \page -> do
    let layout = page ^?! prop "layout" . jsonString
    writeTemplate (getTemplate layout) $ pure $ page
      & prop "url" . jsonString %~ drop 6
      & prop "event" . prop "time" %~ parseEventTime

  copyFilesWith (drop 7) [ "static/*" ]

  pure ()

getTemplate :: String -> FilePath
getTemplate = ("templates/" ++) . (++ ".html")

------------------------------------------------------------------------------
-- | This is a 'Prism'' but I couldn't figure out how to make it typecheck as
-- such.
prop :: Applicative f => Text -> (Value -> f Value) -> Value -> f Value
prop p = _Object . at p . _Just

jsonString :: Prism' Value String
jsonString = _String . _Text

parseEventTime :: Value -> Value
parseEventTime v =
  let str = v ^?! jsonString
      Just (lt, _) = strptime "%Y-%m-%d %H:%M" str
   in object
        [ "dow"  .= jsonString # getDayOfWeek lt
        , "date" .= jsonString # getDate lt
        , "time" .= jsonString # getTime lt
        ]

getDate :: LocalTime -> String
getDate ld =
  let (_, month, day) = toGregorian $ localDay ld
      showMonth =
        case month of
          1  -> "Jan"
          2  -> "Feb"
          3  -> "Mar"
          4  -> "Apr"
          5  -> "May"
          6  -> "Jun"
          7  -> "Jul"
          8  -> "Aug"
          9  -> "Sep"
          10 -> "Oct"
          11 -> "Nov"
          12 -> "Dec"
   in showMonth ++ " " ++ show day

getTime :: LocalTime -> String
getTime ld =
  let TimeOfDay hour minute _ = localTimeOfDay ld
      isPM = hour >= 12
   in if minute /= 0
         then error "we can't show minutes!!"
         else show (hour - bool 0 12 (hour >= 13)) ++ bool "am" "pm" isPM



getDayOfWeek :: LocalTime -> String
getDayOfWeek ld =
  case toWeekDate (localDay ld) ^. _3 of
    1 -> "Mon"
    2 -> "Tue"
    3 -> "Wed"
    4 -> "Thu"
    5 -> "Fri"
    6 -> "Sat"
    7 -> "Sun"


