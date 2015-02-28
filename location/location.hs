-- Parses Google location data and yields information about the time spent
-- at that location
--
{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Data.Maybe
import Geo.Computations
import Data.Aeson
import Data.Ord (comparing)
import Data.List (sort, sortBy, groupBy)
import Control.Applicative
import Control.Monad (liftM, liftM2, mzero)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map

data Location = Location {
    latlong :: Point,
    timestamp :: UTCTime
} deriving Show

data Locations = Locations { locations :: [Location] } deriving Show

instance FromJSON Locations where
    parseJSON (Object v) = Locations <$> v .: "locations"
    parseJSON _          = mzero


parseStrMs :: String -> UTCTime
parseStrMs s = posixSecondsToUTCTime $ (fromInteger $ read s) / 1000

parsePoint :: Integer -> Integer -> Point
parsePoint lon lat = Point (fromInteger lon/10**7)  (fromInteger lat/10**7) Nothing Nothing

instance FromJSON Location where
    parseJSON (Object v) = Location <$>
                           liftM2 parsePoint (v .: "latitudeE7") (v .: "longitudeE7") <*>
                           liftM parseStrMs (v .: "timestampMs")
    parseJSON _          = mzero


locationsByDay :: [Location] -> Map.Map Data.Time.Calendar.Day [Location]
locationsByDay locations = Map.fromList [ ((locationDay (head l)), l) | l <- sortedGroupedLocations]
    where
        sortWithinGroups :: [[Location]] -> [[Location]]
        sortWithinGroups = map (sortBy (comparing timestamp))

        sortGroups :: [[Location]] -> [[Location]]
        sortGroups = sortBy (comparing (timestamp . head))

        sortedGroupedLocations :: [[Location]]
        sortedGroupedLocations = (sortGroups . sortWithinGroups) groupedLocations

        groupedLocations :: [[Location]]
        groupedLocations = groupBy (\x y -> locationDay x == locationDay y) locations

        locationDay :: Location -> Data.Time.Calendar.Day
        locationDay = utctDay . timestamp

locationDistance :: Point -> Location -> Distance
locationDistance p x = distance p (latlong x)

isInWork :: Point -> [Location] -> Bool
isInWork workPoint = any (<100) . map (locationDistance workPoint)

timeStartingWork :: Point -> [Location] -> UTCTime
timeStartingWork workPoint = head . (map timestamp) . filter (\x -> locationDistance workPoint x < 100)

timeLeavingWork :: Point -> [Location] -> UTCTime
timeLeavingWork workPoint = last . (map timestamp) . filter (\x -> locationDistance workPoint x < 100)

timeAtWork :: Point -> [Location] -> UTCTime
timeAtWork workPoint l = posixSecondsToUTCTime $ diffUTCTime (timeLeavingWork workPoint l) (timeStartingWork workPoint l)

fmt :: (Data.Time.Calendar.Day, UTCTime) -> String
fmt (day,time) = (showGregorian day) ++ " " ++ (hrFormat time)
    where
        hrFormat = (formatTime defaultTimeLocale "%H:%M")

main :: IO ()
main = do
  args <- getArgs
  let workLat = read $ args !! 0 :: Double
  let workLong = read $ args !! 1 :: Double
  let workPoint = Point workLat workLong Nothing Nothing

  file <- ByteString.getContents
  let locs = fromJust (decode file :: Maybe Locations)
  let locsByDay = locationsByDay (locations locs)
  let workDayTimes = Map.map (timeAtWork workPoint) $ Map.filter (isInWork workPoint) locsByDay
  putStr $ unlines $ map fmt $ Map.toList workDayTimes
