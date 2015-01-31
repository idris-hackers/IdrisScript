module Main

import IdrisScript
import IdrisScript.Date

main : JS_IO ()
main = do
  date <- now
  log date
  date `setFullYear` (MkYear 2016)
  log date
