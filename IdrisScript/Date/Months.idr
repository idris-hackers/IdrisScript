module IdrisScript.Date.Months

%access public

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

instance Eq Month where
  January   == January   = True
  February  == February  = True
  March     == March     = True
  April     == April     = True
  May       == May       = True
  June      == June      = True
  July      == July      = True
  August    == August    = True
  September == September = True
  October   == October   = True
  November  == November  = True
  December  == December  = True
  _         == _         = False
