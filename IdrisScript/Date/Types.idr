module IdrisScript.Date.Types

import IdrisScript.Date.Months
import IdrisScript.Date.Days

%access public

record Year where
  constructor MkYear
  unYear : Int

instance Eq Year where
  year == year' = unYear year == unYear year'

record Date where
  constructor MkDate
  unDate : Int

instance Eq Date where
  date == date' = unDate date == unDate date'

record Hours where
  constructor MkHours
  unHours : Int

instance Eq Hours where
  hours == hours' = unHours hours == unHours hours'

record Minutes where
  constructor MkMinutes
  unMinutes : Int

instance Eq Minutes where
  mins == mins' = unMinutes mins == unMinutes mins'

record Seconds where
  constructor MkSeconds
  unSeconds : Int

instance Eq Seconds where
  secs == secs' = unSeconds secs == unSeconds secs'

record Milliseconds where
  constructor MkMilliseconds
  unMilliseconds : Int

instance Eq Milliseconds where
  millis == millis' = unMilliseconds millis == unMilliseconds millis'
