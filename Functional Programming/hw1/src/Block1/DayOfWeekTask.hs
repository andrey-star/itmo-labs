{-# LANGUAGE InstanceSigs #-}

-- | Contains functions to work with days of the week
module Block1.DayOfWeekTask
  ( DayOfWeek (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

-- | Day of week representation
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq DayOfWeek where
  (==) :: DayOfWeek -> DayOfWeek -> Bool
  (==) Monday Monday       = True
  (==) Tuesday Tuesday     = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday   = True
  (==) Friday Friday       = True
  (==) Saturday Saturday   = True
  (==) Sunday Sunday       = True
  (==) _ _                 = False

-- | Returns the next day of the week
nextDay :: DayOfWeek -> DayOfWeek
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- | Returns day of week after given number of days
afterDays :: DayOfWeek -> Natural -> DayOfWeek
afterDays day 0 = day
afterDays day x = afterDays (nextDay day) (x - 1)

-- | Returns whether the provided day of week is a weekend
isWeekend :: DayOfWeek -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Returns number of days until Friday
daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1
