module Date where

import Locale
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Data.Maybe

data Date = Date Day deriving (Show, Read, Eq, Ord)
dateFormat = "%-d.%-m.%Y"



printableDate :: Maybe Date -> String
printableDate Nothing = ""
printableDate (Just (Date x)) = formatTime defaultTimeLocale dateFormat x


stringToDate x
  | isJust res = Just $ Date $ fromJust res
  | otherwise = Nothing
  where
    res = parseTimeM True defaultTimeLocale dateFormat x :: Maybe Day




isAnniversary :: Date -> Integer -> Integer -> Bool
isAnniversary (Date date) day month = (dateDay == day) && (dateMonth == month) where
  (_, m, d) = toGregorian date
  dateMonth = toInteger m
  dateDay = toInteger d

    
