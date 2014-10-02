module IdrisScript.Date

import IdrisScript

%access public

namespace Month
  data Month = January
             | February
             | March
             | April
             | May
             | June
             | July
             | August
             | September
             | October
             | November
             | December

namespace Day
  data Day = Monday
           | Tuesday
           | Wednesday
           | Thursday
           | Friday
           | Saturday
           | Sunday

instance Eq Day where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False


instance Cast Day.Day Int where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7

instance Cast Day.Day Integer where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7

instance Cast Day.Day Nat where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7

data Year         = MkYear Int
data Month        = MkMonth Month.Month
data Date         = MkDate Int
data Day          = MkDay Day.Day
data Hours        = MkHours Int
data Minutes      = MkMinutes Int
data Seconds      = MkSeconds Int
data Milliseconds = MkMilliseconds Int

JSDate : JSType
JSDate = JSObject "Date"

now : IO (JSValue JSDate)
now = do
  res <- mkForeign (FFun "new Date()" [] FPtr)
  return $ MkJSObject res

newFromMilliseconds : Milliseconds -> IO (JSValue JSDate)
newFromMilliseconds (MkMilliseconds millis) = do
  res <- mkForeign (FFun "new Date(%0)" [FInt] FPtr) millis
  return $ MkJSObject res

newFromString : String -> IO (JSValue JSDate)
newFromString str = do
  res <- mkForeign (FFun "new Date(%0)" [FString] FPtr) str
  return $ MkJSObject res

getDay : JSValue JSDate -> IO Day.Day
getDay date = do
  day <- mkForeign (FFun "%0.getDay()" [FPtr] FInt) (unpack date)
  return $ toDay day
where
  toDay : Int -> Day.Day
  toDay 1 = Monday
  toDay 2 = Tuesday
  toDay 3 = Wednesday
  toDay 4 = Thursday
  toDay 5 = Friday
  toDay 6 = Saturday
  toDay _ = Sunday

getMilliseconds : JSValue JSDate -> IO Milliseconds
getMilliseconds date = do
  millis <- mkForeign (FFun "%0.getMilliseconds()" [FPtr] FInt) (unpack date)
  return $ MkMilliseconds millis

getMonth : JSValue JSDate -> IO Date.Month
getMonth date  = do
  month <- mkForeign (FFun "%0.getMonth()" [FPtr] FInt) (unpack date)
  return $ MkMonth (toMonth month)
where
  toMonth : Int -> Month.Month
  toMonth 1  = January
  toMonth 2  = February
  toMonth 3  = March
  toMonth 4  = April
  toMonth 5  = May
  toMonth 6  = June
  toMonth 7  = July
  toMonth 8  = August
  toMonth 9  = September
  toMonth 10 = October
  toMonth 11 = November
  toMonth _  = December

getYear : JSValue JSDate -> IO Year
getYear date = do
  year <- mkForeign (FFun "%0.getYear()" [FPtr] FInt) (unpack date)
  return $ MkYear year

getDate : JSValue JSDate -> IO Date
getDate date = do
  date' <- mkForeign (FFun "%0.getDate()" [FPtr] FInt) (unpack date)
  return $ MkDate date'

getHours : JSValue JSDate -> IO Hours
getHours date = do
  hours <- mkForeign (FFun "%0.getHours()" [FPtr] FInt) (unpack date)
  return $ MkHours hours

getMinutes : JSValue JSDate -> IO Minutes
getMinutes date = do
  minutes <- mkForeign (FFun "%0.getMinutes()" [FPtr] FInt) (unpack date)
  return $ MkMinutes minutes

getSeconds : JSValue JSDate -> IO Seconds
getSeconds date = do
  seconds <- mkForeign (FFun "%0.getSeconds()" [FPtr] FInt) (unpack date)
  return $ MkSeconds seconds
