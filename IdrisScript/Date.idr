module IdrisScript.Date

import IdrisScript
import IdrisScript.Date.Months
import IdrisScript.Date.Days
import IdrisScript.Date.Types

%access public

Date : IO (JSValue JSFunction)
Date = do
  date <- mkForeign (FFun "Date" [] FPtr)
  return $ MkJSFunction date

JSDate : JSType
JSDate = JSObject "Date"

||| Creates a new `Date` object with the current time.
now : IO (JSValue JSDate)
now = do
  res <- mkForeign (FFun "new Date()" [] FPtr)
  return $ MkJSObject res

||| Creates a new `Date` object from milliseconds.
newDateFromMilliseconds : Milliseconds -> IO (JSValue JSDate)
newDateFromMilliseconds (MkMilliseconds millis) = do
  res <- mkForeign (FFun "new Date(%0)" [FInt] FPtr) millis
  return $ MkJSObject res

||| Creates a new `Date` object from a string.
newDateFromString : String -> IO (JSValue JSDate)
newDateFromString str = do
  res <- mkForeign (FFun "new Date(%0)" [FString] FPtr) str
  return $ MkJSObject res

||| Copies a `Date` object.
copyDate : (JSValue JSDate) -> IO (JSValue JSDate)
copyDate date = do
  res <- mkForeign (FFun "new Date(%0)" [FPtr] FPtr) (unpack date)
  return $ MkJSObject res

||| Gets the day of `Date` object.
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

||| Gets the milliseconds from a `Date` object.
getMilliseconds : JSValue JSDate -> IO Milliseconds
getMilliseconds date = do
  millis <- mkForeign (FFun "%0.getMilliseconds()" [FPtr] FInt) (unpack date)
  return $ MkMilliseconds millis

||| Gets the month from a `Date` object.
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

||| Gets the year from a `Date` object.
getYear : JSValue JSDate -> IO Int
getYear date = mkForeign (FFun "%0.getYear()" [FPtr] FInt) (unpack date)

||| Sets the year from a `Date` object. Modifies the original date.
setYear : JSValue JSDate -> Year -> IO (JSValue JSDate)
setYear date year = do
  mkForeign (
      FFun "%0.setYear(%1)" [FPtr, FInt] FUnit
    ) (unpack date) (unYear year)
  return date

||| Gets the date from a `Date` object.
getDate : JSValue JSDate -> IO Date
getDate date = do
  date' <- mkForeign (FFun "%0.getDate()" [FPtr] FInt) (unpack date)
  return $ MkDate date'

||| Sets the date from a `Date` object. Modifies the original date.
setDate : JSValue JSDate -> Date -> IO (JSValue JSDate)
setDate date date' = do
  mkForeign (
      FFun "%0.setDate(%1)" [FPtr, FInt] FInt
    ) (unpack date) (unDate date')
  return date

||| Gets the hours from a `Date` object.
getHours : JSValue JSDate -> IO Hours
getHours date = do
  hours <- mkForeign (FFun "%0.getHours()" [FPtr] FInt) (unpack date)
  return $ MkHours hours

||| Sets the hours from a `Date` object. Modifies the original date.
setHours : JSValue JSDate -> Hours -> IO (JSValue JSDate)
setHours date hours = do
  mkForeign (
      FFun "%0.setHours(%1)" [FPtr, FInt] FInt
    ) (unpack date) (unHours hours)
  return date

||| Gets the minutes from a `Date` object.
getMinutes : JSValue JSDate -> IO Minutes
getMinutes date = do
  minutes <- mkForeign (FFun "%0.getMinutes()" [FPtr] FInt) (unpack date)
  return $ MkMinutes minutes

||| Sets the minutes from a `Date` object. Modifies the original date.
setMinutes : JSValue JSDate -> Minutes -> IO (JSValue JSDate)
setMinutes date mins = do
  mkForeign (
      FFun "%0.setMinutes(%1)" [FPtr, FInt] FInt
    ) (unpack date) (unMinutes mins)
  return date

||| Gets the seconds from a `Date` object.
getSeconds : JSValue JSDate -> IO Seconds
getSeconds date = do
  seconds <- mkForeign (FFun "%0.getSeconds()" [FPtr] FInt) (unpack date)
  return $ MkSeconds seconds

||| Sets the seconds from a `Date` object. Modifies the original date.
setSeconds : JSValue JSDate -> Seconds -> IO (JSValue JSDate)
setSeconds date secs = do
  mkForeign (
      FFun "%0.setSeconds(%1)" [FPtr, FInt] FInt
    ) (unpack date) (unSeconds secs)
  return date

||| Gets the full year of a `Date` object.
getFullYear : JSValue JSDate -> IO Year
getFullYear date = do
  year <- mkForeign (FFun "%0.getFullYear()" [FPtr] FInt) (unpack date)
  return $ MkYear year

||| Sets the full year of a `Date` object. Modifies the original date.
setFullYear : JSValue JSDate -> Year -> IO (JSValue JSDate)
setFullYear date year = do
  mkForeign (
      FFun "%0.setFullYear(%1)" [FPtr, FInt] FInt
    ) (unpack date) (unYear year)
  return date
