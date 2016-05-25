module IdrisScript.Date.Types

import IdrisScript.Date.Months
import IdrisScript.Date.Days

%access public export

record Year where
  constructor MkYear
  unYear : Int

implementation Eq Year where
  year == year' = unYear year == unYear year'

record Date where
  constructor MkDate
  unDate : Int

implementation Eq Date where
  date == date' = unDate date == unDate date'

record Hours where
  constructor MkHours
  unHours : Int

implementation Eq Hours where
  hours == hours' = unHours hours == unHours hours'

record Minutes where
  constructor MkMinutes
  unMinutes : Int

implementation Eq Minutes where
  mins == mins' = unMinutes mins == unMinutes mins'

record Seconds where
  constructor MkSeconds
  unSeconds : Int

implementation Eq Seconds where
  secs == secs' = unSeconds secs == unSeconds secs'

record Milliseconds where
  constructor MkMilliseconds
  unMilliseconds : Int

implementation Eq Milliseconds where
  millis == millis' = unMilliseconds millis == unMilliseconds millis'
