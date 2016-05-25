module IdrisScript.Date

import IdrisScript
import public IdrisScript.Date.Months
import public IdrisScript.Date.Days
import public IdrisScript.Date.Types

%access public export

Date : JS_IO (JSValue JSFunction)
Date = do
  date <- jscall  "Date" (JS_IO Ptr)
  return $ MkJSFunction date

JSDate : JSType
JSDate = JSObject "Date"

||| Creates a new `Date` object with the current time.
now : JS_IO (JSValue JSDate)
now = do
  res <- jscall "new Date()" (JS_IO Ptr)
  return $ MkJSObject res

||| Creates a new `Date` object from milliseconds.
newDateFromMilliseconds : Milliseconds -> JS_IO (JSValue JSDate)
newDateFromMilliseconds (MkMilliseconds millis) = do
  res <- jscall "new Date(%0)" (Int -> JS_IO Ptr) millis
  return $ MkJSObject res

||| Creates a new `Date` object from a string.
newDateFromString : String -> JS_IO (JSValue JSDate)
newDateFromString str = do
  res <- jscall "new Date(%0)" (String -> JS_IO Ptr) str
  return $ MkJSObject res

||| Copies a `Date` object.
copyDate : (JSValue JSDate) -> JS_IO (JSValue JSDate)
copyDate date = do
  res <- jscall "new Date(%0)" (Ptr -> JS_IO Ptr) (unpack date)
  return $ MkJSObject res

||| Gets the day of `Date` object.
getDay : JSValue JSDate -> JS_IO Day
getDay date = do
  day <- jscall "%0.getDay()" (Ptr -> JS_IO Int) (unpack date)
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
getMilliseconds : JSValue JSDate -> JS_IO Milliseconds
getMilliseconds date = do
  millis <- jscall "%0.getMilliseconds()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkMilliseconds millis

||| Sets the milliseconds from a `Date` object. Modifies the original date.
setMilliseconds : JSValue JSDate -> Milliseconds -> JS_IO (JSValue JSDate)
setMilliseconds date millis = do
  jscall "%0.getMilliseconds(%1)" (Ptr -> Int -> JS_IO Int)
             (unpack date) (unMilliseconds millis)
  return date

||| Gets the month from a `Date` object.
getMonth : JSValue JSDate -> JS_IO Month
getMonth date  = do
  month <- jscall "%0.getMonth()" (Ptr -> JS_IO Int) (unpack date)
  return $ toMonth month
where
  toMonth : Int -> Month
  toMonth 0  = January
  toMonth 1  = February
  toMonth 2  = March
  toMonth 3  = April
  toMonth 4  = May
  toMonth 5  = June
  toMonth 6  = July
  toMonth 7  = August
  toMonth 8  = September
  toMonth 9  = October
  toMonth 10 = November
  toMonth _  = December

||| Sets the month from a `Date` object. Modifies the original date.
setMonth : JSValue JSDate -> Month -> JS_IO (JSValue JSDate)
setMonth date month = do
  jscall "%0.setMonth(%1)" (Ptr -> Int -> JS_IO Int)
           (unpack date) (fromMonth month)
  return date
where
  fromMonth : Month -> Int
  fromMonth January   = 0
  fromMonth February  = 1
  fromMonth March     = 2
  fromMonth April     = 3
  fromMonth May       = 4
  fromMonth June      = 5
  fromMonth July      = 6
  fromMonth August    = 7
  fromMonth September = 8
  fromMonth October   = 9
  fromMonth November  = 10
  fromMonth December  = 11

||| Gets the year from a `Date` object.
getYear : JSValue JSDate -> JS_IO Int
getYear date = jscall "%0.getYear()" (Ptr -> JS_IO Int) (unpack date)

||| Sets the year from a `Date` object. Modifies the original date.
setYear : JSValue JSDate -> Year -> JS_IO (JSValue JSDate)
setYear date year = do
  jscall "%0.setYear(%1)" (Ptr -> Int -> JS_IO Int)
         (unpack date) (unYear year)
  return date

||| Gets the date from a `Date` object.
getDate : JSValue JSDate -> JS_IO Date
getDate date = do
  date' <- jscall "%0.getDate()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkDate date'

||| Sets the date from a `Date` object. Modifies the original date.
setDate : JSValue JSDate -> Date -> JS_IO (JSValue JSDate)
setDate date date' = do
  jscall "%0.setDate(%1)" (Ptr -> Int -> JS_IO Int)
          (unpack date) (unDate date')
  return date

||| Gets the hours from a `Date` object.
getHours : JSValue JSDate -> JS_IO Hours
getHours date = do
  hours <- jscall "%0.getHours()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkHours hours

||| Sets the hours from a `Date` object. Modifies the original date.
setHours : JSValue JSDate -> Hours -> JS_IO (JSValue JSDate)
setHours date hours = do
  jscall "%0.setHours(%1)" (Ptr -> Int -> JS_IO Int)
         (unpack date) (unHours hours)
  return date

||| Gets the minutes from a `Date` object.
getMinutes : JSValue JSDate -> JS_IO Minutes
getMinutes date = do
  minutes <- jscall "%0.getMinutes()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkMinutes minutes

||| Sets the minutes from a `Date` object. Modifies the original date.
setMinutes : JSValue JSDate -> Minutes -> JS_IO (JSValue JSDate)
setMinutes date mins = do
  jscall "%0.setMinutes(%1)" (Ptr -> Int -> JS_IO Int)
         (unpack date) (unMinutes mins)
  return date

||| Gets the seconds from a `Date` object.
getSeconds : JSValue JSDate -> JS_IO Seconds
getSeconds date = do
  seconds <- jscall "%0.getSeconds()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkSeconds seconds

||| Sets the seconds from a `Date` object. Modifies the original date.
setSeconds : JSValue JSDate -> Seconds -> JS_IO (JSValue JSDate)
setSeconds date secs = do
  jscall "%0.setSeconds(%1)" (Ptr -> Int -> JS_IO Int)
         (unpack date) (unSeconds secs)
  return date

||| Gets the full year of a `Date` object.
getFullYear : JSValue JSDate -> JS_IO Year
getFullYear date = do
  year <- jscall "%0.getFullYear()" (Ptr -> JS_IO Int) (unpack date)
  return $ MkYear year

||| Sets the full year of a `Date` object. Modifies the original date.
setFullYear : JSValue JSDate -> Year -> JS_IO (JSValue JSDate)
setFullYear date year = do
  jscall "%0.setFullYear(%1)" (Ptr -> Int -> JS_IO Int)
         (unpack date) (unYear year)
  return date
