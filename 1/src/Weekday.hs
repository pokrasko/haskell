{-# LANGUAGE DeriveGeneric #-}

module Weekday where

import           GHC.Generics (Generic)
import           Test.QuickCheck.Arbitrary     (Arbitrary(..))
import           Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary, genericArbitrary)


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
               deriving (Eq, Generic, Show)

instance Arbitrary Weekday where
    arbitrary = genericArbitrary

instance ToADTArbitrary Weekday

-- Enum, succ
nextDay :: Weekday -> Weekday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

afterDays :: Int -> Weekday -> Weekday
afterDays n
    | n < 0   = error "Weekday.afterDays: negative number"
    | n >= 7  = afterDays (n `mod` 7)
afterDays 0   = id
afterDays n   = afterDays (n - 1) . nextDay

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Weekday -> Int
daysToParty = daysToParty' 0
  where
    daysToParty' n Friday = n
    daysToParty' n w      = daysToParty' (n + 1) $ nextDay w
