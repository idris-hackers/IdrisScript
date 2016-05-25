module IdrisScript.Date.Days

import IdrisScript

%access public export

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday

implementation Eq Day where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

implementation Cast Day Int where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7

implementation Cast Day Integer where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7

implementation Cast Day Nat where
  cast Monday    = 1
  cast Tuesday   = 2
  cast Wednesday = 3
  cast Thursday  = 4
  cast Friday    = 5
  cast Saturday  = 6
  cast Sunday    = 7
