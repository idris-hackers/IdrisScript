module IdrisScript.Date

import IdrisScript
import IdrisScript.Date.Months
import IdrisScript.Date.Days
import IdrisScript.Date.Types

%access public


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

getDay : JSValue JSDate -> IO Day
getDay date = do
  day <- mkForeign (FFun "%0.getDay()" [FPtr] FInt) (unpack date)
  return $ toDay day
where
  toDay : Int -> Day
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

getMonth : JSValue JSDate -> IO Month
getMonth date  = do
  month <- mkForeign (FFun "%0.getMonth()" [FPtr] FInt) (unpack date)
  return $ toMonth month
where
  toMonth : Int -> Month
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

getYear : JSValue JSDate -> IO Int
getYear date = mkForeign (FFun "%0.getYear()" [FPtr] FInt) (unpack date)

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

getFullYear : JSValue JSDate -> IO Year
getFullYear date = do
  year <- mkForeign (FFun "%0.getFullYear()" [FPtr] FInt) (unpack date)
  return $ MkYear year
