module Propellor.Property.Scheduled
	( period
	, periodParse
	, Recurrance(..)
	, WeekDay
	, MonthDay
	, YearDay
	) where

import Propellor
import Utility.Scheduled

import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as M

-- | Makes a Property only be checked every so often.
--
-- This uses the description of the Property to keep track of when it was
-- last run.
period :: Property -> Recurrance -> Property
period prop recurrance = flip describe desc $ adjustProperty prop $ \satisfy -> do
	lasttime <- liftIO $ getLastChecked (propertyDesc prop)
	nexttime <- liftIO $ fmap startTime <$> nextTime schedule lasttime
	t <- liftIO localNow
	if Just t >= nexttime
		then do
			r <- satisfy
			liftIO $ setLastChecked t (propertyDesc prop)
			return r
		else noChange
  where
	schedule = Schedule recurrance AnyTime
	desc = propertyDesc prop ++ " (period " ++ fromRecurrance recurrance ++ ")"

-- | Like period, but parse a human-friendly string.
periodParse :: Property -> String -> Property
periodParse prop s = case toRecurrance s of
	Just recurrance -> period prop recurrance
	Nothing -> property "periodParse" $ do
		liftIO $ warningMessage $ "failed periodParse: " ++ s
		noChange

lastCheckedFile :: FilePath
lastCheckedFile = localdir </> ".lastchecked"

getLastChecked :: Desc -> IO (Maybe LocalTime)
getLastChecked desc = M.lookup desc <$> readLastChecked

localNow :: IO LocalTime
localNow = do
	now <- getCurrentTime
	tz <- getTimeZone now
	return $ utcToLocalTime tz now

setLastChecked :: LocalTime -> Desc -> IO ()
setLastChecked time desc = do
	m <- readLastChecked
	writeLastChecked (M.insert desc time m)

readLastChecked :: IO (M.Map Desc LocalTime)
readLastChecked = fromMaybe M.empty <$> catchDefaultIO Nothing go
  where
	go = readish <$> readFileStrict lastCheckedFile

writeLastChecked :: M.Map Desc LocalTime -> IO ()
writeLastChecked = writeFile lastCheckedFile . show
