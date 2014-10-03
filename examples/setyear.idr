module Main

import IdrisScript
import IdrisScript.Date

main : IO ()
main = do
  date <- now
  log date
  date `setFullYear` (MkYear 2015)
  log date
