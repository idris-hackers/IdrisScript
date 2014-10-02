module IdrisScript.Date.Types

import IdrisScript.Date.Months
import IdrisScript.Date.Days

%access public

record Year : Type where
  MkYear : (unYear : Int) -> Year

instance Eq Year where
  year == year' = unYear year == unYear year'

record Date : Type where
  MkDate : (unDate : Int) -> Date

instance Eq Date where
  date == date' = unDate date == unDate date'

record Hours : Type where
  MkHours : (unHours : Int) -> Hours

instance Eq Hours where
  hours == hours' = unHours hours == unHours hours'

record Minutes : Type where
  MkMinutes : (unMinutes : Int) -> Minutes

instance Eq Minutes where
  mins == mins' = unMinutes mins == unMinutes mins'

record Seconds : Type where
  MkSeconds : (unSeconds : Int) -> Seconds

instance Eq Seconds where
  secs == secs' = unSeconds secs == unSeconds secs'

record Milliseconds : Type where
  MkMilliseconds : (unMilliseconds : Int) -> Milliseconds

instance Eq Milliseconds where
  millis == millis' = unMilliseconds millis == unMilliseconds millis'
